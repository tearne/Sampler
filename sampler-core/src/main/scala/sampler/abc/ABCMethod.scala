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

import scala.annotation.tailrec
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.slf4j.LoggerFactory
import sampler.data.Empirical
import sampler.Implicits._
import sampler.math.Probability
import sampler.math.Random
import sampler.abc.population.PopulationBuilder
import sampler.math.StatisticsComponent
import sampler.abc.population.EncapsulatedPopulation

class ABCMethod(meta: ABCParameters, random: Random) extends Serializable{
	val log = LoggerFactory.getLogger(this.getClass)
	
	def init[M <: ABCModel](model: M): EncapsulatedPopulation[M] = {
		val numParticles = meta.numParticles
		val pop0 = (1 to numParticles).par.map(i => Particle(model.prior.sample(), 1.0, Double.MaxValue)).seq
		EncapsulatedPopulation(model)(pop0)
	}
	
	protected def evolveOnce[M <: ABCModel](
			ePop: EncapsulatedPopulation[M], 
			pBuilder: PopulationBuilder,
			tolerance: Double
	): Option[EncapsulatedPopulation[M]] = {
		// Number of particles to be generated per job?
		val jobSizes = (1 to meta.numParticles)
			.grouped(meta.particleChunking)
			.map(_.size).toList
		log.info(s"Tolerance = $tolerance, Job sizes = $jobSizes")
		
		import ePop.model._
		val prevPop = ePop.population
		val results: Seq[Try[Population]] = pBuilder.run(ePop.model)(ePop.population, jobSizes, tolerance, meta, random)
		
		val failures = results.collect{
			case Failure(e: RefinementAbortedException) => Right(e)
			case Failure(e) => Left(e)
		}
		
		failures.collectFirst{case Left(e) => throw e}			
		
		val exceptions = results.collect{case Failure(e) => e}
		if(failures.size > 0) {
			log.warn("{} exception(s) thrown building population.  First: {}", failures.size, failures(0))
			None
		}
	    else Some{
	    	EncapsulatedPopulation(ePop.model)(results.collect{case Success(s) => s}.flatten)
	    }
	}
		
	def run[M <: ABCModel with StatisticsComponent](
			pop: EncapsulatedPopulation[M], 
			pBuilder: PopulationBuilder
	): Option[EncapsulatedPopulation[M]] = {
		@tailrec
		def refine(
				ePop: EncapsulatedPopulation[M], 
				numAttempts: Int, 
				currentTolerance: Double,
				previousTolerance: Double
		): Option[EncapsulatedPopulation[M]] = {
			log.info(numAttempts + " generations remaining")
			//TODO report a failure ratio at the end of a generation
			if(numAttempts == 0) Some(pop)
			else{
				evolveOnce(pop, pBuilder, currentTolerance) match {
					case None =>
						log.warn(s"Failed to refine current population, evolving within previous tolerance $previousTolerance")
						refine(pop, numAttempts - 1, previousTolerance, previousTolerance)
					case Some(newEPop) =>
						//Next tolerance is the median of the previous best for each particle
						val fit = newEPop.population.map(_.bestFit)
						val medianTolerance = ePop.model.quantile(newEPop.population.map(_.bestFit).toEmpiricalSeq, Probability(0.5))
						val newTolerance = 
							if(medianTolerance == 0) {
								log.warn("Median tolerance from last generation evaluated to 0, half the previous tolerance will be used instead.")
								currentTolerance / 2
							}
							else medianTolerance
						refine(newEPop, numAttempts - 1, newTolerance, currentTolerance)
				}
			}
		}
		
		refine(pop, meta.refinements, meta.tolerance,  meta.tolerance)
	}
}
