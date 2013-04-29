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
package sampler.examples

import sampler.abc.{ABCModel, ABCMethod}
import sampler.math.Random
import sampler.abc.ABCMeta
import sampler.abc.Prior
import sampler.data.Samplable
import sampler.run.SerialRunner
import sampler.r.ScriptRunner
import sampler.data.Empirical._
import sampler.math.Probability
import flanagan.integration.DerivnFunction
import flanagan.integration.RungeKutta
import java.nio.file.{Paths, Files}
import sampler.data.Types.Column
import sampler.io.CSVTableWriter
import sampler.math.StatisticsComponent
import sampler.run.Aborter

object FlockMortality extends App{
	import FlockMortalityModel._
	val wd = Paths.get("egout","FlockMortality")
	Files.createDirectories(wd)
	
	val abcMethod = new ABCMethod(FlockMortalityModel)
	val encapPopulation0 = abcMethod.init
	implicit val modelRandom = FlockMortalityModel.modelRandom 
	val finalPopulation = abcMethod.run(encapPopulation0, SerialRunner()).get.map(_.value)//.population

	val posteriors = Map(
		"Beta" -> finalPopulation.map(_.beta).toEmpiricalSeq,
		"Eta" -> finalPopulation.map(_.eta).toEmpiricalSeq,
		"Gamma" -> finalPopulation.map(_.gamma).toEmpiricalSeq,
		"Sigma" -> finalPopulation.map(_.sigma).toEmpiricalSeq,
		"Sigma2" -> finalPopulation.map(_.sigma2).toEmpiricalSeq
	)
	
	val csvName = "results.csv"
	
	new CSVTableWriter(wd.resolve("results.csv"), overwrite = true).apply(
		Column(finalPopulation.map(_.beta), "Beta"),
		Column(finalPopulation.map(_.eta), "Eta"),	
		Column(finalPopulation.map(_.gamma), "Gamma"),
		Column(finalPopulation.map(_.delta), "Delta"),
		Column(finalPopulation.map(_.sigma), "Sigma"),
		Column(finalPopulation.map(_.sigma2), "Sigma2"),
		Column(finalPopulation.map(_.offset), "Offset")
	)
	
	new CSVTableWriter(wd.resolve("obseravtions.csv"), overwrite = true).apply(
		Column(observations.dailyEggs, "Eggs"),
		Column(observations.dailyDead, "Dead")
	)
	
	
	//Get median fit data
	val half = Probability(0.5)
	val stats = StatisticsComponent
	val medBeta = stats.quantile(finalPopulation.map(_.beta).toEmpiricalSeq, half)
	val medEta = stats.quantile(finalPopulation.map(_.eta).toEmpiricalSeq, half)
	val medGamma = stats.quantile(finalPopulation.map(_.gamma).toEmpiricalSeq, half)
	val medDelta = stats.quantile(finalPopulation.map(_.delta).toEmpiricalSeq, half)
	val medSigma = stats.quantile(finalPopulation.map(_.sigma).toEmpiricalSeq, half)
	val medSigma2 = stats.quantile(finalPopulation.map(_.sigma2).toEmpiricalSeq, half)
	val medOffset = stats.quantile(finalPopulation.map(_.offset).map(_.toDouble).toEmpiricalTable, half).toInt
	
	val medParams = Parameters(medBeta, medEta, medGamma, medDelta, medSigma, medSigma2, medOffset)
	val fitted = samplableModel(medParams, observations).sample
	
	val days = 0 until observations.dailyDead.size
	println(days)
	val cumulativeObsDead = observations.dailyDead.scanLeft(0){case (a,v)=> a + v.toInt}.tail
//	val cumulativeObsEggs = observations.dailyEggs.scanLeft(0){case (a,v)=> a + v.toInt}.tail
//	val cumulativeFitEggs = days.map(i => fitted.dayStates.mapValues(_.eggs)(i)).scanLeft(0){case (a,v)=> a + v.toInt}.tail
	
	println(fitted.dayStates)
	
	new CSVTableWriter(wd.resolve("fitted.csv"), overwrite = true).apply(
		Column(days, "Day"),
		Column(days.map(i => fitted.dayStates.mapValues(_.eggs)(i)), "FitEggs"),
		Column(days.map(i => fitted.dayStates.mapValues(_.d)(i)), "FitDead"),
		Column(observations.dailyEggs, "ObsEggs"),
		Column(cumulativeObsDead, "ObsDead")
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

//TODO name doesn't match file
object FlockMortalityModel extends ABCModel{
	val statistics = new StatisticsComponent {}
	implicit val abcRandom = Random
	val modelRandom = Random
	
	val observations = Observations(
		dailyEggs = List(2200,2578,2654,2167,2210,2444,2182,2152,2208,2100,1644,1872,1092,1236,1116,1200,1025,1172,1096,1122),
		dailyDead = List(0,0,0,0,0,0,0,1,15,70,39,60,74,46,54,25,5,5,6,1)
	)
	
	val flockSize = 3000
	val eggCoef = 2200.0 / 3000.0
	
	val meta = new ABCMeta(
    	reps = 1,
		numParticles = 100, 
		refinements = 50,
		particleRetries = 100, 
		particleChunking = 100
	)
	
	case class Parameters(
			beta: Double, 
			eta: Double, 
			gamma: Double, 
			delta: Double, 
			sigma: Double, 
			sigma2: Double, 
			offset: Int
	) extends ParametersBase {
		val kernel = Samplable.normal(0, 0.1)
		val threeDie = Samplable.uniform(IndexedSeq(-1,0,1))
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
	}
	case class Observations(dailyEggs: List[Int], dailyDead: List[Int]) extends ObservationsBase
	
	case class Output(dayStates: Map[Int, ODEState]) extends OutputBase{
		def distanceTo(obs: Observations) = {
			
			
			val accumulatedSimDead = (0 until obs.dailyDead.length).map(i => dayStates(i).d.toInt) //No Sum since already accumulated when simulated
		    val accumulatedObsDead = obs.dailyDead.scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    
		    val accumulatedSimEggs = (0 until obs.dailyEggs.length).map(i => dayStates(i).eggs).scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    val accumulatedObsEggs = obs.dailyEggs.scanLeft(0){case (a,v)=> a + v.toInt}.tail
		    
		    val deadMax = accumulatedObsDead.last
		    //val eggsMax = accumulatedObsEggs.last
		    val eggsMax = obs.dailyEggs.max
		    //val deadMax = obs.dailyDead.max
		    
		    val newObsDead = accumulatedObsDead map (i => i.toDouble / deadMax)
		    val newSimDead = accumulatedSimDead map (i => i.toDouble / deadMax)
		    
//		    val newObsEggs = accumulatedObsEggs map (i => i.toDouble / eggsMax)
//		    val newSimEggs = accumulatedSimEggs map (i => i.toDouble / eggsMax)
		    
		    val newObsEggs = obs.dailyEggs map (i => i.toDouble / eggsMax)
		    val newSimEggs = (0 until obs.dailyEggs.length).map(i => dayStates(i).eggs) map (i => i.toDouble / eggsMax)

//		    val newObsDead = obs.dailyDead map (i => i.toDouble / deadMax)
//		    val newSimDead = (0 until obs.dailyDead.length).map(i => dayStates(i).eggs) map (i => i.toDouble / deadMax)

		    
		    val error = (0 until newObsDead.length).foldLeft(0.0){case (acc,dayIndex) =>
		    	val delta = math.pow(newSimDead(dayIndex) - newObsDead(dayIndex), 2) +
							math.pow(newSimEggs(dayIndex) - newObsEggs(dayIndex), 2)
				acc+delta
		    }
		    
		    error
		}
	}
	
	def samplableModel(p: Parameters, obs: Observations) = {
		import p._
		val days = 0 until obs.dailyDead.length
		
		def solve(): Output = {
			class SEIRDerivn extends DerivnFunction {
				def derivn(x: Double, y: Array[Double]): Array[Double] = {
					if(y.size != 5)	throw new UnsupportedOperationException("Y should have length 5.");
					  
					val dr = Array.fill(y.length)(0.0);
				  
					dr(0) = -beta * y(0) * y(2) 						//S: -beta S I
					dr(1) = beta * y(0) * y(2) - eta * y(1)				//E: beta S I - gamma E
					dr(2) = eta * y(1) - gamma * y(2) - delta * y(2)	//I: eta I - gamma I - delta I
					dr(3) = gamma * y(2)								//R: gamma I
					dr(4) = delta * y(2)								//D: the observed data
							  
					dr
				}
			}
			
			val timeZero = 0.0
			val relTol = 1e-11; val absTol = 1e-11
			val defaultStepSize = 0.1
			val initialCondition = Array((flockSize-1).toDouble, 1.0, 0.0, 0.0, 0.0) //S, E, I, R, D
					  
			def getResultArrOnDay(day: Int) = RungeKutta.cashKarp(
				new SEIRDerivn(),
				0.0,
				initialCondition,
				day,
				defaultStepSize,
				absTol,
				relTol
			)
							  
			val resultsArrays = days map(getResultArrOnDay _)
			val unshiftedResultsMap = days.foldLeft(Map[Int, ODEState]()){case (map, day) => {
				val array = resultsArrays(day)
				map.updated(day, ODEState(
					array(0),	//S 
					array(1), 	//E
					array(2), 	//I
					array(3), 	//R
					array(4),	//D
					eggCoef*(array(0)+array(1))+sigma*array(2)+sigma2*array(3) //Eggs			
				))			 
			}}
					  
			val shiftedResultsMap =
				if(offset == 0) unshiftedResultsMap
				else if(offset > 0){
					val tmp = unshiftedResultsMap.map{case (key, value) =>
					(key + offset) -> value
					}
					val additions = (0 until offset).foldLeft(Map[Int, ODEState]()){case (map, day) =>
					map.updated(day, ODEState(
						flockSize,
						0,
						0,
						0,
						0,
						observations.dailyEggs(0)))
					}
					tmp.++(additions)
				}
				else throw new RuntimeException("not supported yet")

			Output(shiftedResultsMap)
		}
		
		Samplable.diracDelta(solve)
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