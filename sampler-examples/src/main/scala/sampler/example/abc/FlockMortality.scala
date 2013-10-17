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
import sampler.abc.ABCMethod
import sampler.abc.ABCModel
import sampler.abc.ABCParameters
import sampler.abc.Prior
import sampler.abc.population.LocalPopulationBuilder
import sampler.data.Distribution
import sampler.io.CSVFile
import sampler.math.Probability
import sampler.math.Random
import sampler.r.ScriptRunner
import sampler.math.StatisticsComponent

object FlockMortality extends App{
	import FlockMortalityModel._
	val wd = Paths.get("results").resolve("FlockMortality")
	Files.createDirectories(wd)
	
	val abcParams = new ABCParameters(
    	reps = 1,
		numParticles = 1000, 
		refinements = 12,
		particleRetries = 1000, 
		particleChunking = 200
	)
	val abcMethod = new ABCMethod(FlockMortalityModel, abcParams, Random)
	val encapPopulation0 = abcMethod.init
	
	implicit val modelRandom = FlockMortalityModel.modelRandom 
	val finalPopulation = abcMethod.run(encapPopulation0, LocalPopulationBuilder()).get.map(_.value)//.population

	val posteriors = Map(
		"Beta" -> finalPopulation.map(_.beta).toEmpiricalSeq,
		"Eta" -> finalPopulation.map(_.eta).toEmpiricalSeq,
		"Gamma" -> finalPopulation.map(_.gamma).toEmpiricalSeq,
		"Sigma" -> finalPopulation.map(_.sigma).toEmpiricalSeq,
		"Sigma2" -> finalPopulation.map(_.sigma2).toEmpiricalSeq
	)
	
	val csvName = "results.csv"
	
	CSVFile.write(
			wd.resolve("obseravtions.csv"), 
			observations.dailyEggs.zip(observations.dailyEggs).map{case (e,d) => s"$e,$d"}, 
			overwrite = true, 
			header = Seq("Eggs", "Dead")
	)
	
	
	CSVFile.write(
			wd.resolve(csvName),
			finalPopulation.map{_.toCSV},
			overwrite = true,
			header = Parameters.header
	)
	
	//Get median fit data
	val half = Probability(0.5)
	val medBeta = quantile(finalPopulation.map(_.beta).toEmpiricalSeq, half)
	val medEta = quantile(finalPopulation.map(_.eta).toEmpiricalSeq, half)
	val medGamma = quantile(finalPopulation.map(_.gamma).toEmpiricalSeq, half)
	val medDelta = quantile(finalPopulation.map(_.delta).toEmpiricalSeq, half)
	val medSigma = quantile(finalPopulation.map(_.sigma).toEmpiricalSeq, half)
	val medSigma2 = quantile(finalPopulation.map(_.sigma2).toEmpiricalSeq, half)
	val medOffset = quantile(finalPopulation.map(_.offset).map(_.toDouble).toEmpiricalTable, half).toInt
	
	val medParams = Parameters(medBeta, medEta, medGamma, medDelta, medSigma, medSigma2, medOffset)
	val fitted = modelDistribution(medParams, observations).sample
	
	val days = 0 until observations.dailyDead.size
	println(days)
	val cumulativeObsDead = observations.dailyDead.scanLeft(0){case (a,v)=> a + v.toInt}.tail
	
	case class Fitted(day: Int, fitEggs: Double, fitDead: Double, obsEggs: Int, obsDead: Int){
		def toCSV = s"$day, $fitEggs, $fitDead, $obsEggs, $obsDead"
	}
	object Fitted{
		def header = Seq("Day", "FitEggs", "FitDead", "ObsEggs", "ObsDead")
	}
	
	val fittedData = days.map{day =>Fitted(
		day,
		fitted.dayStates.mapValues(_.eggs)(day),
		fitted.dayStates.mapValues(_.d)(day),
		observations.dailyEggs(day),
		cumulativeObsDead(day)
	)}
	
	CSVFile.write(
			wd.resolve("fitted.csv"),
			fittedData.map(_.toCSV),
			overwrite = true,
			header = Fitted.header
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

object FlockMortalityModel extends ABCModel with StatisticsComponent{
	implicit val abcRandom = Random
	val modelRandom = Random
	val statistics = sampler.math.Statistics
	
	val observations = Observations(
		dailyEggs = List(2200,2578,2654,2167,2210,2444,2182,2152,2208,2100,1644,1872,1092,1236,1116,1200,1025,1172,1096,1122),
		dailyDead = List(0,0,0,0,0,0,0,1,15,70,39,60,74,46,54,25,5,5,6,1)
	)
	
	val flockSize = 3000
	val eggCoef = 2200.0 / 3000.0
	
	case class Parameters(
			beta: Double, 
			eta: Double, 
			gamma: Double, 
			delta: Double, 
			sigma: Double, 
			sigma2: Double, 
			offset: Int
	) extends ParametersBase {
		val kernel = new Prior[Double] with Distribution[Double]{
			val normal = new NormalDistribution(0,0.1)
			def sample = normal.sample
			def density(at: Double) = normal.density(at)
		}
		val threeDie = Distribution.uniform(IndexedSeq(-1,0,1))
		private def threeDensity(v: Int) = if(v <= 1 || v >= -1) 1.0 / 3 else 0
		
		def perturb = Parameters(
			beta + kernel.sample,
			eta + kernel.sample,
			gamma + kernel.sample,
			delta + kernel.sample,
			sigma + kernel.sample,
			sigma2 + kernel.sample,
			offset + threeDie.sample
		)
		def perturbDensity(that: Parameters) = 
			kernel.density(beta - that.beta) *
			kernel.density(eta - that.eta) *
			kernel.density(gamma - that.gamma) *
			kernel.density(delta - that.delta) *
			kernel.density(sigma - that.sigma) *
			kernel.density(sigma2 - that.sigma2) *
			threeDensity(offset - that.offset)
			
		def toCSV = s"$beta, $eta, $gamma, $delta, $sigma, $sigma2, $offset" 
	}
	object Parameters{
		val header = Seq("Beta", "Eta", "Gamma", "Delta", "Sigma", "Sigma2", "Offset")
	}
	case class Observations(dailyEggs: List[Int], dailyDead: List[Int]) extends ObservationsBase
	
	case class Output(dayStates: Map[Int, ODEState]) extends OutputBase{
		def distanceTo(obs: Observations) = {
			val accumulatedSimDead = (0 until obs.dailyDead.length).map(i => dayStates(i).d.toInt) //No Sum since already accumulated when simulated
		    val accumulatedObsDead = obs.dailyDead.scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    
		    val accumulatedSimEggs = (0 until obs.dailyEggs.length).map(i => dayStates(i).eggs).scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    val accumulatedObsEggs = obs.dailyEggs.scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    
		    val deadMax = accumulatedObsDead.last
		    val eggsMax = obs.dailyEggs.max
		    
		    val newObsDead = accumulatedObsDead map (i => i.toDouble / deadMax)
		    val newSimDead = accumulatedSimDead map (i => i.toDouble / deadMax)
		    
		    val newObsEggs = obs.dailyEggs map (i => i.toDouble / eggsMax)
		    val newSimEggs = (0 until obs.dailyEggs.length).map(i => dayStates(i).eggs) map (i => i.toDouble / eggsMax)
		    
		    val error = (0 until newObsDead.length).foldLeft(0.0){case (acc,dayIndex) =>
		    	val delta = math.pow(newSimDead(dayIndex) - newObsDead(dayIndex), 2) +
							math.pow(newSimEggs(dayIndex) - newObsEggs(dayIndex), 2)
				acc+delta
		    }
		    
		    error
		}
	}
	
	def modelDistribution(p: Parameters, obs: Observations) = {
		import p._
		val days = 0 until obs.dailyDead.length
		
		def solve(): Output = {
			val timeZero = 0.0
			val relTol = 1e-11; val absTol = 1e-11
			val minStep = 1e-8; val maxStep = 100.0
			val y0 = Array((flockSize-1).toDouble, 1.0, 0.0, 0.0, 0.0) //S, E, I, R, D
			val numDays = obs.dailyDead.size	
			
			class ODE(p: Parameters) extends FirstOrderDifferentialEquations {
				import p._
				
			    def getDimension() = 5
			    def computeDerivatives(time: Double, y: Array[Double], yDot: Array[Double]) {
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
			        		eggCoef*(rounded(0)+rounded(1))+sigma*rounded(2)+sigma2*rounded(3)
			        )
			    	steps.append(state)
			    }
			}
			val stepNormaliser = new StepNormalizer(1.0, stepHandler)
			
			val dp853 = new DormandPrince853Integrator(minStep, maxStep, absTol, relTol)
			var out = Array[Double](0,0,0,0,0) //Not really used, since the step hander gets the output
			dp853.addStepHandler(stepNormaliser)
			dp853.integrate(new ODE(p), 0.0, y0, numDays, out)
		
			val unshiftedResultsMap = days.foldLeft(Map[Int, ODEState]()){case (map, day) =>
				map.updated(day,steps(day))			 
			}
					  
			val shiftedResultsMap =
				if(offset == 0) unshiftedResultsMap
				else if(offset > 0){
					val tmp = unshiftedResultsMap.map{case (key, value) =>
						(key + offset) -> value
					}
					val additions = (0 until offset).foldLeft(Map[Int, ODEState]()){case (map, day) =>
						map.updated(day, ODEState(flockSize,0,0,0,0,observations.dailyEggs(0)))
					}
					tmp.++(additions)
				}
				else throw new RuntimeException("not supported yet")

			Output(shiftedResultsMap)
		}
		
		//Deterministic model will always give the same answer
		Distribution.continually(solve)
	}
	
	val prior = new Prior[Parameters]{
		def density(p: Parameters) = {
			def unitRange(d: Double) = if(d > 1.0 || d < 0.0) 0.0 else 1.0
			def tenRange(i: Int) = if(i < 0 || i > 10) 0.0 else 1.0
			
			import p._
			val d = unitRange(beta) *
			unitRange(eta) *
			unitRange(gamma) *
			unitRange(delta) *
			unitRange(sigma) *
			unitRange(sigma2) *
			tenRange(offset)
			
			d
		}
		
		//TODO can use random in the model?
		def sample = Parameters(
			beta = modelRandom.nextDouble(0, 1),
			eta = modelRandom.nextDouble(0, 1),
			gamma = modelRandom.nextDouble(0, 1),
			delta = modelRandom.nextDouble(0, 1),
			sigma = modelRandom.nextDouble(0, 1),
			sigma2 = modelRandom.nextDouble(0, 1),
			offset = modelRandom.nextInt(10)
		)
	}
}

case class ODEState(s: Double, e: Double, i: Double, r: Double, d: Double, eggs: Double)