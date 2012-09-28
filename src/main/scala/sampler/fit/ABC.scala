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

import sampler.data._
import sampler.data.Types._
import sampler.math._
import scala.annotation.tailrec
import sampler.data.WeightsTable
import java.nio.file.Path
import sampler.io.CSVTableWriter
import sampler.run.JobRunner
import sampler.run.AbortableRunner
import java.util.concurrent.atomic.AtomicBoolean
import sampler.run.Abort
import sampler.run.AbortableJob
import sampler.data.FrequencyTableBuilderComponent

trait Prior[A] extends Samplable[A]{
	def density(value: A): Double
}

trait ABCModel{
	type Parameters <: ParametersBase
	protected trait ParametersBase {
		def perturb(): Parameters
		def perturbDensity(that: Parameters): Double		
	}
	
	type Observations <: ObservationsBase
	protected trait ObservationsBase
	
	type Output <: OutputBase
	protected trait OutputBase {
		def closeToObserved(observed: Observations, tolerance: Double): Boolean
	}
	
	def init(p: Parameters, obs: Observations): Samplable[Output]
	
	trait PopulationWriter{
		def apply(population: WeightsTable[Parameters], tolerance: Double): Unit
	}
}

trait ABCComponent{
	this: FrequencyTableBuilderComponent with
		  StatisticsComponent =>
	
	def apply(model: ABCModel, r: Random)( 
			prior: Prior[model.Parameters],
			obs: model.Observations, 
			reps: Int, 
			particles: Int, 
			startTolerance: Double,
			refinementAttempts: Int,
			runner: AbortableRunner,
			writer: Option[model.PopulationWriter] = None
	): WeightsTable[model.Parameters] = {
		type P = model.Parameters
		
		val popZero: WeightsTable[P] = WeightsTable(
			(1 to particles).par.map(i => Particle(prior.sample(r), 1.0)).seq
		)
		
		def evolve(population: WeightsTable[P], tolerance: Double): Option[WeightsTable[P]] = {
			println("Now working on tolerance = "+tolerance)
			
			def getNextParticle(keepGoing: AtomicBoolean): Option[Particle[P]] = {
				val start = System.currentTimeMillis()

				@tailrec
				def tryParticle(failures: Int): Option[Particle[P]] = {
					if(!keepGoing.get) None
					else if(failures == 1e2) {
						println("returning None")
						None
					}
					else{
						val candidate = population.sample(r).value.perturb
						val assessedModel = model.init(candidate, obs).map(_.closeToObserved(obs, tolerance))(r)
						
						//val numSuccess = statistics.occursCount(builder(assessedModel)(_.size == reps)(r), true)
						
//						val numSuccess = FrequencyTableBuilder
//							.serial(assessedModel)(_.size == reps)(r)
//							.samples.count(identity) //TODO use a counting statistic?
						val fHat = builder(assessedModel)(_.size == reps)(r).probabilityMap(true).value//numSuccess.toDouble / reps
			
						val res = if(fHat > 0){
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
				
				val res = tryParticle(0)
				val end = System.currentTimeMillis()
				res
			}
			
			//TODO JobRunner Abortable Job syntax too noisy
			val results: Seq[Option[Particle[P]]] = runner(
					Abort[Particle[P]](_.contains(None))
			){
					// population.particles replaced population.values when deleted
					val jobs = population.particles.map(particle => AbortableJob[Particle[P]](stillRunning => getNextParticle(stillRunning)))
					jobs.toSeq
			}
			
			val newPopulation = results.flatten
			if(newPopulation.size == results.size) Some(WeightsTable(newPopulation))
			else None
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
						refine(newPop, numAttempts - 1, tolerance * decentFactor, tolerance, decentFactor)
					}
				}
			}
		}
		
		refine(popZero, refinementAttempts, startTolerance, startTolerance, 0.5)
	}
}