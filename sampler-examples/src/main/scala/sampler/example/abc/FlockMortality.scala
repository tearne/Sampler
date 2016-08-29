/*
 * Copyright (c) 2012 Crown Copyright 
 *                    Animal Health and Veterinary Laboratories Agency
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package sampler.example.abc

import java.math.MathContext
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import org.apache.commons.math3.ode.sampling.{FixedStepHandler, StepNormalizer}
import org.apache.commons.math3.random.{MersenneTwister, RandomGenerator, SynchronizedRandomGenerator}
import play.api.libs.json.Json
import sampler._
import sampler.abc.{ABC, ABCConfig, Model, Prior}
import sampler.distribution.Distribution
import sampler.io.{CSV, Tokenable, Tokens}
import sampler.maths.Random
import sampler.r.script.RScript

import scala.Array.canBuildFrom
import scala.collection.mutable.Buffer
import scala.language.existentials

object FlockMortality extends App {
	import FlockMortalityModel._
	
	val wd = Paths.get("results").resolve("FlockMortality")
	Files.createDirectories(wd)
	implicit val r = Random
	
	val abcConfig = ABCConfig(ConfigFactory.load.getConfig("flock-mortality-example"))
	val posterior = ABC(FlockMortalityModel, abcConfig)
	
	val writer = Files.newBufferedWriter(wd.resolve("posterior.json"), Charset.defaultCharset())
	writer.write(Json.prettyPrint(posterior.toJSON()))
	writer.close()
	
	CSV.writeLines(
			wd.resolve("obseravtions.csv"), 
			Seq("Eggs", "Dead") +: observed.dailyEggs.zip(observed.dailyEggs).map(_.productIterator.toSeq) 
	)

	//Get median fit data
	val posteriorSamplable = Distribution.fromWeightsTable(posterior.consolidatedWeightsTable)
	val bunchOfSamples = (1 to 100000).map(_ => posteriorSamplable.sample)
	val medBeta = bunchOfSamples.map(_.beta).toEmpirical.percentile(0.5)
	val medEta = bunchOfSamples.map(_.eta).toEmpirical.percentile(0.5)
	val medGamma = bunchOfSamples.map(_.gamma).toEmpirical.percentile(0.5)
	val medDelta = bunchOfSamples.map(_.delta).toEmpirical.percentile(0.5)
	val medSigma = bunchOfSamples.map(_.sigma).toEmpirical.percentile(0.5)
	val medSigma2 = bunchOfSamples.map(_.sigma2).toEmpirical.percentile(0.5)
	val medOffset = bunchOfSamples.map(_.offset.toDouble).groupBy(identity).mapValues(_.size).toEmpirical.percentile(0.5).toInt
	
	val medianParams = FlockMortalityParams(medBeta, medEta, medGamma, medDelta, medSigma, medSigma2, medOffset)
	val fitted = modelDistribution(medianParams).sample
	
	val days = 0 until observed.dailyDead.size
	val cumulativeObsDead = observed.dailyDead.scanLeft(0){case (a,v)=> a + v.toInt}.tail
	
	case class Fitted(day: Int, fitEggs: Double, fitDead: Double, obsEggs: Int, obsDead: Int){
		def toSeq = Seq(day, fitEggs, fitDead, obsEggs, obsDead)
	}
	object Fitted{
		def header = Seq("Day", "FitEggs", "FitDead", "ObsEggs", "ObsDead")
	}
	
	val fittedData = days.map{day =>Fitted(
		day,
		fitted.dayStates.mapValues(_.eggs)(day),
		fitted.dayStates.mapValues(_.d)(day),
		observed.dailyEggs(day),
		cumulativeObsDead(day)
	)}
	
	CSV.writeLines(
			wd.resolve("fitted.csv"),
			Fitted.header +: fittedData.map(_.toSeq)
	)
	
	val rScript = 
"""
lapply(c("ggplot2", "reshape", "jsonlite"), require, character.only=T)

posterior = as.data.frame(fromJSON("posterior.json")$particles)
observations = read.csv("obseravtions.csv")
fitted = read.csv("fitted.csv")

sampledPosterior = posterior[sample(nrow(posterior), replace = T, 1000, prob = posterior$weight),]

pdf("density.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(subset(sampledPosterior, select = c(-offset, -weight))), aes(x=value, colour=variable)) + geom_density()
ggplot(melt(sampledPosterior[,"offset"]), aes(x=factor(value))) + 
	geom_bar() +
	scale_x_discrete("Offset (days)")
ggplot(fitted, aes(x=Day)) + 
		geom_line(aes(y=FitEggs)) +
		geom_point(aes(y=ObsEggs)) +
		scale_y_continuous("Egg Production")
ggplot(fitted, aes(x=Day)) +
		geom_line(aes(y=FitDead)) +
		geom_point(aes(y=ObsDead)) +
		scale_y_continuous("Dead")

dev.off()
"""

	RScript(rScript, wd.resolve("script.r"))
}

case class FlockMortalityParams(
			beta: Double, 
			eta: Double, 
			gamma: Double, 
			delta: Double, 
			sigma: Double, 
			sigma2: Double, 
			offset: Int
){
	def toSeq = Seq(beta, eta, gamma, delta, sigma, sigma2, offset) 
}
object FlockMortalityParams{
  implicit val tokenableInstance: Tokenable[FlockMortalityParams] = new Tokenable[FlockMortalityParams]{
	  val mc = new MathContext(6)
    def getTokens(p: FlockMortalityParams) = Tokens.named(
      "beta" ->   BigDecimal(p.beta, mc),
      "eta" ->    BigDecimal(p.eta, mc),
      "gamma" ->  BigDecimal(p.gamma, mc),
      "delta" ->  BigDecimal(p.delta, mc),
      "sigma" ->  BigDecimal(p.sigma, mc),
      "sigma2" -> BigDecimal(p.sigma2, mc),
      "offset" -> BigDecimal(p.offset, mc)
		)
  }
}

object FlockMortalityModel extends Model[FlockMortalityParams] {
	val observed = Observed(
		dailyEggs = List(2200,2578,2654,2167,2210,2444,2182,2152,2208,2100,1644,1872,1092,1236,1116,1200,1025,1172,1096,1122),
		dailyDead = List(0,0,0,0,0,0,0,1,15,70,39,60,74,46,54,25,5,5,6,1)
	)
	
	val flockSize = 3000
	val eggCoef = 2200.0 / 3000.0
	
	val prior = new Prior[FlockMortalityParams]{
		def density(p: FlockMortalityParams) = {
			def unitRange(d: Double) = if(d > 1.0 || d < 0.0) 0.0 else 1.0
			def tenRange(i: Int) = if(i < 0 || i > 10) 0.0 else 1.0
			
			import p._
			unitRange(beta) *
			unitRange(eta) *
			unitRange(gamma) *
			unitRange(delta) *
			unitRange(sigma) *
			unitRange(sigma2) *
			tenRange(offset)
		}

		def draw(r: Random) = FlockMortalityParams(
			beta = r.nextDouble(0, 1),
			eta = r.nextDouble(0, 1),
			gamma = r.nextDouble(0, 1),
			delta = r.nextDouble(0, 1),
			sigma = r.nextDouble(0, 1),
			sigma2 = r.nextDouble(0, 1),
			offset = r.nextInt(10)
		)
	}
	
	private val kernel = new Prior[Double] with Distribution[Double]{
		/*
		 * The Mersenne Twister is a fast generator with very good properties well suited for Monte-Carlo simulation
		 * http://commons.apache.org/proper/commons-math/userguide/random.html
		 */
		val normal = {
			val syncRand: RandomGenerator = new SynchronizedRandomGenerator(new MersenneTwister())
			new NormalDistribution(syncRand, 0, 0.1, NormalDistribution.DEFAULT_INVERSE_ABSOLUTE_ACCURACY)
		}
		def draw(r: Random) = {
			val r = normal.sample
			
			if(r.isNaN() || r.isInfinite()) {
				val e = new Exception("here... r = "+r)
				e.printStackTrace()
				throw e
			}
			
			r
		}
		def density(at: Double) = {
			normal.density(at)
		}
	}
	val threeDie = Distribution.uniform(IndexedSeq(-1,0,1))
	private def threeDensity(v: Int) = if(v <= 1 || v >= -1) 1.0 / 3 else 0
	
	def perturb(p: FlockMortalityParams) = {
		import p._
    implicit val random = Random
		FlockMortalityParams(
			beta + kernel.sample,
			eta + kernel.sample,
			gamma + kernel.sample,
			delta + kernel.sample,
			sigma + kernel.sample,
			sigma2 + kernel.sample,
			offset + threeDie.sample
		)
	}
	def perturbDensity(a: FlockMortalityParams, b: FlockMortalityParams) = 
		kernel.density(a.beta - b.beta) *
		kernel.density(a.eta - b.eta) *
		kernel.density(a.gamma - b.gamma) *
		kernel.density(a.delta - b.delta) *
		kernel.density(a.sigma - b.sigma) *
		kernel.density(a.sigma2 - b.sigma2) *
		threeDensity(a.offset - b.offset)
	object Parameters{
		val header = Seq("Beta", "Eta", "Gamma", "Delta", "Sigma", "Sigma2", "Offset")
	}
	
	def distanceToObservations(p: FlockMortalityParams) = modelDistribution(p).map(_.distanceToObserved)
	
	case class Observed(dailyEggs: List[Int], dailyDead: List[Int])
	case class Simulated(dayStates: Map[Int, ODEState]) {
		def distanceToObserved: Double = {
			val accumulatedSimDead = (0 until observed.dailyDead.length).map(i => dayStates(i).d.toInt) //No Sum since already accumulated when simulated
		    val accumulatedObsDead = observed.dailyDead.scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    
		    val accumulatedSimEggs = (0 until observed.dailyEggs.length).map(i => dayStates(i).eggs).scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    val accumulatedObsEggs = observed.dailyEggs.scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    
		    val deadMax = accumulatedObsDead.last
		    val eggsMax = observed.dailyEggs.max
		    
		    val newObsDead = accumulatedObsDead map (i => i.toDouble / deadMax)
		    val newSimDead = accumulatedSimDead map (i => i.toDouble / deadMax)
		    
		    val newObsEggs = observed.dailyEggs map (i => i.toDouble / eggsMax)
		    val newSimEggs = (0 until observed.dailyEggs.length).map(i => dayStates(i).eggs) map (i => i.toDouble / eggsMax)
		    
		    val error = (0 until newObsDead.length).foldLeft(0.0){case (acc,dayIndex) =>
		    	val delta = math.pow(newSimDead(dayIndex) - newObsDead(dayIndex), 2) +
							math.pow(newSimEggs(dayIndex) - newObsEggs(dayIndex), 2)
				acc+delta
		    }
		    
		    error
		}
	}
	
	def modelDistribution(p: FlockMortalityParams) = {
		val days = 0 until observed.dailyDead.length
		
		def solve(): Simulated = {
			val timeZero = 0.0
			val relTol = 1e-11; val absTol = 1e-11
			val minStep = 1e-8; val maxStep = 100.0
			val y0 = Array((flockSize-1).toDouble, 1.0, 0.0, 0.0, 0.0) //S, E, I, R, D
			val numDays = observed.dailyDead.size	
			
			class ODE() extends FirstOrderDifferentialEquations {
				
			    def getDimension() = 5
			    def computeDerivatives(time: Double, y: Array[Double], yDot: Array[Double]) {
			    	import p._
			    	yDot(0) = -beta * y(0) * y(2) 						//S: -beta S I
					yDot(1) = beta * y(0) * y(2) - eta * y(1)			//E: beta S I - gamma E
					yDot(2) = eta * y(1) - gamma * y(2) - delta * y(2)	//I: eta I - gamma I - delta I
					yDot(3) = gamma * y(2)								//R: gamma I
					yDot(4) = delta * y(2)								//D: the observed dead data
			    }
			}
			
			val steps = Buffer[ODEState]()
			
			val stepHandler = new FixedStepHandler() {
			    def init(t0: Double, y0: Array[Double], t: Double) {}
			    def handleStep(t: Double, y: Array[Double], yDot: Array[Double], isLast: Boolean) {
            val rounded = y.map{value => if(math.abs(value) < 1e-4) 0 else value }
			    	val state = ODEState(
			        		rounded(0), 
			        		rounded(1), 
			        		rounded(2), 
			        		rounded(3), 
			        		rounded(4), 
			        		eggCoef*(rounded(0)+rounded(1))+p.sigma*rounded(2)+p.sigma2*rounded(3)
			        )
			    	steps.append(state)
			    }
			}
			val stepNormaliser = new StepNormalizer(1.0, stepHandler)
			
			val dp853 = new DormandPrince853Integrator(minStep, maxStep, absTol, relTol)
			val out = Array[Double](0,0,0,0,0) //Not really used, since the step hander gets the output
			dp853.addStepHandler(stepNormaliser)
			dp853.integrate(new ODE(), 0.0, y0, numDays, out)
			val integrated = steps.toList
			
			val unshiftedResultsMap = days.foldLeft(Map[Int, ODEState]()){case (map, day) =>
				map.updated(day,integrated(day))			 
			}
					  
			val shiftedResultsMap =
				if(p.offset == 0) unshiftedResultsMap
				else if(p.offset > 0){
					val tmp = unshiftedResultsMap.map{case (key, value) =>
						(key + p.offset) -> value
					}
					val additions = (0 until p.offset).foldLeft(Map[Int, ODEState]()){case (map, day) =>
						map.updated(day, ODEState(flockSize,0,0,0,0,observed.dailyEggs(0)))
					}
					tmp.++(additions)
				}
				else throw new RuntimeException("not supported yet")

			Simulated(shiftedResultsMap)
		}
		
		//Deterministic model will always return the same answer
		Distribution.always(solve)
	}
}

case class ODEState(s: Double, e: Double, i: Double, r: Double, d: Double, eggs: Double)