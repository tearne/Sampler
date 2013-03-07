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

package sampler.abc

import sampler.math.Random
import scala.annotation.tailrec
import sampler.data.Empirical._
import sampler.run.JobRunner
import sampler.run.Job
import sampler.math.Probability

object ABCMethod{
	import sampler.data.SerialSampleBuilder
	
	def init[R <: Random](model: ABCModel[R]): EncapsulatedPopulation[R] = {
		val numParticles = model.meta.numParticles
		val initialPopulation = (1 to numParticles).par.map(i => Particle(model.prior.sample(model.random), 1.0, Double.MaxValue)).seq
		EncapsulatedPopulation(model)(initialPopulation)
	}
	
	def generateParticles[R <: Random](
			ePop: EncapsulatedPopulation[R], 
			quantity: Int, 
			tolerance: Double
	): Option[EncapsulatedPopulation[R]] = {
		type Params = ePop.model.Parameters
		import ePop.model._
		import ePop.population
		
		@tailrec
		def nextParticle(failures: Int = 0): Option[Particle[Params]] = {
			//if(!keepGoing.get) None
			//else  
			if(failures >= meta.particleRetries) {
				println(s"Failed after $failures trials")
				None
			}
			else{
				//println(s"attempt $failures")
				def getScores(params: Params) = {
					val modelWithMetric = samplableModel(params, observations).map(_.distanceTo(observations))
					val modelWithScores = SerialSampleBuilder(modelWithMetric)(_.size == meta.reps)(random)
						.filter(_ <= tolerance)
					modelWithScores
				}
				
				def getWeight(params: Parameters, numPassed: Int) = {
					val fHat = numPassed.toDouble / meta.reps
					val numerator = fHat * prior.density(params)
					val denominator = population.map{case Particle(value, weight, bestScore) => 
						weight * value.perturbDensity(params)
					}.sum
					if(numerator > 0 && denominator > 0) Some(numerator / denominator)
					else None	
				}
				
				//TODO inefficient to do this every time
				val samplable = population.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
				
				val res = for{
					params <- Some(samplable.sample(random).perturb(random)) if prior.density(params) > 0
					fitScores <- Some(getScores(params))// if scores.size > 0
					//_ = println("---"+fitScores)
					weight <- getWeight(params, fitScores.size) 
				} yield(Particle(params, weight, fitScores.min))
				
				res match {
					case s: Some[Particle[Parameters]] => s
					case None => nextParticle(failures + 1)
				}
			}
		}
		
		val particles = (1 to quantity)
			.view
			.map(i => nextParticle())
			.takeWhile(_.isDefined)
			.map(_.get)
			.force
		
		println("Num particles = "+particles.size)
		
		if(particles.size == quantity) Some(ePop.update(particles))
		else None
	}
	
	def evolveOnce[R <: Random](
			ePop: EncapsulatedPopulation[R], 
			runner: JobRunner,
			tolerance: Double
	): Option[EncapsulatedPopulation[R]] = {
		type Params = ePop.model.Parameters
		import ePop.model
		import model.meta
		
		println("Now working on tolerance = "+tolerance)
		
		// Number of particles to be generated per job?
		val jobSizes = (1 to meta.numParticles)
			.grouped(meta.particleChunking)
			.map(_.size).toList
		println(s"JobSizes: $jobSizes")
		
		val jobs = jobSizes.map(numParticles => Job{() =>
			generateParticles(ePop, numParticles, tolerance)
		}).toList
		println("running jobs")
		val runnerResults: Seq[Option[EncapsulatedPopulation[R]]] = runner.apply(jobs)
		println("done")	
			
		//TODO Don't like 'asInstanceOf' below
		val newParticles = runnerResults.view
			.takeWhile(_.isDefined)
			.map(_.get.population.asInstanceOf[Seq[Particle[Params]]])
			.force
			.flatten
			
		if(newParticles.size == meta.numParticles) Some(ePop.update(newParticles))
		else None
	}
		
	def run[R <: Random](
			ePop: EncapsulatedPopulation[R], 
			runner: JobRunner
	): Option[EncapsulatedPopulation[R]] = {
		type Params = ePop.model.Parameters
		import ePop.model
		import model.meta
		
		@tailrec
		def refine(
				ePop: EncapsulatedPopulation[R], 
				numAttempts: Int, 
				currentTolerance: Double,
				oldTolerance: Double
		): Option[EncapsulatedPopulation[R]] = {
			println("Generations left to go "+numAttempts)
			if(numAttempts == 0) Some(ePop)
			else{
				evolveOnce(ePop, runner, currentTolerance) match {
					case None =>
						println(s"Failed to refine current population, evolving with old tolerance $oldTolerance")
						refine(ePop, numAttempts - 1, oldTolerance, oldTolerance)
					case Some(newEPop) =>
						//Next tolerance is the median of the previous best for each particle
						val medianTolerance = newEPop.population.map(_.bestFit).toEmpiricalSeq.quantile(Probability(0.5))
						refine(newEPop, numAttempts - 1, medianTolerance, currentTolerance)
				}
			}
		}
		
		refine(ePop, meta.refinements, meta.tolerance, meta.tolerance)
	}
}