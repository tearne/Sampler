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

import java.nio.file.Files
import java.nio.file.Paths
import scala.Array.canBuildFrom
import scala.collection.mutable.Buffer
import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import org.apache.commons.math3.ode.sampling.FixedStepHandler
import org.apache.commons.math3.ode.sampling.StepNormalizer
import sampler.Implicits._
import sampler.data.Distribution
import sampler.math.Random
import sampler.r.ScriptRunner
import scala.language.existentials
import sampler.math.Statistics
import sampler.cluster.abc.Model
import sampler.cluster.abc.Prior
import sampler.cluster.abc.config.ABCConfig
import com.typesafe.config.ConfigFactory
import sampler.cluster.abc.ABC
import sampler.io.CSV
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.RandomDataImpl
import org.apache.commons.math3.random.MersenneTwister

object FlockMortality extends App {
	import FlockMortalityModel._
	import Statistics._
	
	val wd = Paths.get("results").resolve("FlockMortality")
	Files.createDirectories(wd)
	implicit val modelRandom = FlockMortalityModel.modelRandom 
	
//	val numParticles = 100
//	val numReplicates = 1
//	val numGenerations = 16
//	val	particleRetries = 1000
//	val	particleChunkSize = 50
	
	val abcParams = ABCConfig.fromTypesafeConfig(
		ConfigFactory.load,
		"flock-mortality-example"
	)
	
	val posterior = ABC(FlockMortalityModel, abcParams)
	
	val posteriors = Map(
		"Beta" -> posterior.map(_.beta).toEmpiricalSeq,
		"Eta" -> posterior.map(_.eta).toEmpiricalSeq,
		"Gamma" -> posterior.map(_.gamma).toEmpiricalSeq,
		"Sigma" -> posterior.map(_.sigma).toEmpiricalSeq,
		"Sigma2" -> posterior.map(_.sigma2).toEmpiricalSeq
	)
	
	val csvName = "results.csv"
	
	CSV.writeLines(
			wd.resolve("obseravtions.csv"), 
			Seq("Eggs", "Dead") +: observed.dailyEggs.zip(observed.dailyEggs).map(_.productIterator.toSeq) 
	)
	
	CSV.writeLines(
			wd.resolve(csvName),
			Parameters.header +: posterior.map(_.toSeq)
	)
	
	//Get median fit data
	val medBeta = quantile(posterior.map(_.beta).toEmpiricalSeq, 0.5)
	val medEta = quantile(posterior.map(_.eta).toEmpiricalSeq, 0.5)
	val medGamma = quantile(posterior.map(_.gamma).toEmpiricalSeq, 0.5)
	val medDelta = quantile(posterior.map(_.delta).toEmpiricalSeq, 0.5)
	val medSigma = quantile(posterior.map(_.sigma).toEmpiricalSeq, 0.5)
	val medSigma2 = quantile(posterior.map(_.sigma2).toEmpiricalSeq, 0.5)
	val medOffset = quantile(posterior.map(_.offset).map(_.toDouble).toEmpiricalTable, 0.5).toInt
	
	val medianParams = FlockMortalityParams(medBeta, medEta, medGamma, medDelta, medSigma, medSigma2, medOffset)
	val fitted = modelDistribution(medianParams).sample
	
	val days = 0 until observed.dailyDead.size
	println(days)
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
s"""
lapply(c("ggplot2", "reshape", "deSolve"), require, character.only=T)

posterior = read.csv("$csvName")
observations = read.csv("obseravtions.csv")
fitted = read.csv("fitted.csv")

pdf("density.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(posterior[,colnames(posterior) != "Offset"]), aes(x=value, colour=variable)) + geom_density()
ggplot(melt(posterior[,"Offset"]), aes(x=factor(value))) + 
	geom_histogram() +
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

	ScriptRunner.apply(rScript, wd.resolve("script.r"))
	
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

object FlockMortalityModel extends Model[FlockMortalityParams] {
	val random = Random
	val modelRandom = Random
	val statistics = sampler.math.Statistics
	
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
		
		//TODO can use random in the model?
		def draw = FlockMortalityParams(
			beta = modelRandom.nextDouble(0, 1),
			eta = modelRandom.nextDouble(0, 1),
			gamma = modelRandom.nextDouble(0, 1),
			delta = modelRandom.nextDouble(0, 1),
			sigma = modelRandom.nextDouble(0, 1),
			sigma2 = modelRandom.nextDouble(0, 1),
			offset = modelRandom.nextInt(10)
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
		def draw = {
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
	val threeDie = Distribution.uniform(IndexedSeq(-1,0,1))(random)
	private def threeDensity(v: Int) = if(v <= 1 || v >= -1) 1.0 / 3 else 0
	
	def perturb(p: FlockMortalityParams) = {
		import p._
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
		Distribution.continually(solve)
	}
}

case class ODEState(s: Double, e: Double, i: Double, r: Double, d: Double, eggs: Double)