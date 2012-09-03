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

package sampler.fit

import sampler.data.Distribution
import sampler.data.WeightsTable
import sampler.data.Particle
import sampler.math.Random
import sampler.data.FrequencyTableBuilder
import scala.annotation.tailrec
import sampler.data.WeightsTable
import java.nio.file.Path
import sampler.io.CSVTableWriter
import sampler.data.Types._
import sampler.run.JobRunner

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
}

trait ABCModel{
	type Parameters <: ParametersBase
	protected trait ParametersBase{
		def perturb(): Parameters
		def perturbDensity(that: Parameters): Double		
	}
	
	type Output <: OutputBase
	protected trait OutputBase {
		def closeToObserved(tolerance: Double): Boolean
	}
	
	def withParameters(p: Parameters): Distribution[Output]
	
	trait PopulationWriter{
		def apply(population: WeightsTable[Parameters], tolerance: Double): Unit
	}
}


object ABC{
	def apply(model: ABCModel, r: Random)( 
			prior: Prior[model.Parameters], 
			reps: Int, 
			particles: Int, 
			startTolerance: Double,
			refinementAttempts: Int,
			runner: JobRunner[Particle[model.Parameters]],
			writer: Option[model.PopulationWriter] = None
	): WeightsTable[model.Parameters] = {
		type P = model.Parameters
		
		val popZero: WeightsTable[P] = WeightsTable(
			(1 to particles).par.map(i => Particle(prior.sample(r), 1.0)).seq
		)
		
		def evolve(population: WeightsTable[P], tolerance: Double): Option[WeightsTable[P]] = {
			println("Now working on tolerance = "+tolerance)
			
			def getNextParticle() = {
				@tailrec
				def tryParticle(failures: Int): Option[Particle[P]] = {
					if(failures > 0 && failures % 1000 == 0) None
					else{
						val candidate = population.sample(r).perturb
						val assessedModel = model.withParameters(candidate).map(_.closeToObserved(tolerance))(r)
						//TODO parallel stuff here?
						val numSuccess = FrequencyTableBuilder
							.serial(assessedModel)(_.size == reps)(r)
							.samples.count(identity) //Pimp to use a counting statistic?
						val fHat = numSuccess.toDouble / reps
			
						val res = if(numSuccess != 0){
							val numerator = fHat * prior.density(candidate)
							val denominator = population.particles.map{p => 
								p.weight * p.value.perturbDensity(candidate)
							}.sum
							if(numerator > 0 && denominator > 0)
								Some(Particle(candidate, numerator / denominator))
							else
								None
						} 
						else None
						
						res match {
							case s: Some[Particle[P]] => s
							case None => tryParticle(failures + 1)
						}
					}
				}
				
				tryParticle(0)
			}
			
			val results: Option[Seq[Option[Particle[P]]]] = runner{
				population.mapValues(_ => getNextParticle _)
			}
			
			results match{
				case None => None
				case Some(particleOpts: Seq[Option[Particle[P]]]) => {
					val particles = particleOpts.flatten
					if(particles.size == results.get.size) Some(WeightsTable(particles))
					else None
				}
			}
		}
		
		@tailrec
		def refine(population: WeightsTable[P], numAttempts: Int, tolerance: Double, lastGoodTolerance: Double, decentFactor: Double): WeightsTable[P] = {
			if(numAttempts == 0) population
			else{
				//TODO on failure, change decent rate so that it persists, rather than just one time
				
				evolve(population, tolerance) match {
					case None => {
						val newDecentFactor = decentFactor + (1 - decentFactor)/2
						val retryTolerance = lastGoodTolerance * newDecentFactor
						println("Retry with decent factor %f, tolerance %f".format(newDecentFactor, retryTolerance))
						refine(population, numAttempts - 1, retryTolerance, lastGoodTolerance, newDecentFactor)
					}
					case Some(newPop) => {
						writer match { 
							case Some(w) => w(newPop, tolerance) 
							case _ =>
						}
//						populationWriter.model.populationWriter(newPop, tolerance)
						refine(newPop, numAttempts - 1, tolerance * decentFactor, tolerance, decentFactor)
					}
				}
			}
		}
		
		refine(popZero, refinementAttempts, startTolerance, startTolerance, 0.5)
	}
}