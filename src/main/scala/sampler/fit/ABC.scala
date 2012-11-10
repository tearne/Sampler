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
import sampler.data.Empirical._
import sampler.data.Types._
import sampler.math._
import scala.annotation.tailrec
import java.nio.file.Path
import sampler.io.CSVTableWriter
import sampler.run.JobRunner
import sampler.run.AbortableRunner
import java.util.concurrent.atomic.AtomicBoolean
import sampler.run.Abort
import sampler.run.AbortableJob

trait Prior[A,Rnd] extends Samplable[A,Rnd]{
	def density(value: A): Double
}

trait ABCModel[Rnd]{
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
	
	def init(p: Parameters, obs: Observations): Samplable[Output,Rnd]
	
	trait PopulationWriter{
		def apply(population: Seq[Parameters], tolerance: Double): Unit
	}
}

trait ABCComponent{
	this: SampleBuilderComponent =>
		
	case class Particle[A](value: A, weight: Double)
	
	def apply[R <: Random](model: ABCModel[R], r: R)( 
			prior: Prior[model.Parameters,R],
			obs: model.Observations, 
			reps: Int, 
			particles: Int, 
			startTolerance: Double,
			refinementAttempts: Int,
			runner: AbortableRunner,
			writer: Option[model.PopulationWriter] = None
	): Seq[model.Parameters] = {
		type P = model.Parameters
		
		val uniformlyWeightedParticles = (1 to particles).par.map(i => Particle(prior.sample(r), 1.0)).seq
		
		def evolve(population: Seq[Particle[P]], tolerance: Double): Option[Seq[Particle[P]]] = {
			println("Now working on tolerance = "+tolerance)
			
			//Map the sequence of weighted params (particles) to a map from param to (summed) weight 
			val samplable = population.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
			
			def getNextParticle(keepGoing: AtomicBoolean): Option[Particle[P]] = {
				@tailrec
				def tryParticle(failures: Int): Option[Particle[P]] = {
					if(!keepGoing.get) None
					else if(failures == 1e2) {
						println("returning None")
						None
					}
					else{
						val candidate = samplable.sample(r).perturb
						val assessedModel = model.init(candidate, obs).map(_.closeToObserved(obs, tolerance))
						
						val numSuccess = builder(assessedModel)(_.size == reps)(r)
							.count(identity) //TODO use a counting statistic?
						val fHat = numSuccess.toDouble / reps
						
						val res = if(fHat > 0){
							//Calculate a weight for this new particle
							val numerator = fHat * prior.density(candidate)
							val denominator = population.map{case Particle(value, weight) => 
								weight * value.perturbDensity(candidate)
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
				res
			}
			
			//TODO JobRunner Abortable Job syntax too noisy
			val results: Seq[Option[Particle[P]]] = runner(
					Abort[Particle[P]](_.contains(None))
			){
					val jobs = (1 to particles).map(particle => AbortableJob[Particle[P]](stillRunning => getNextParticle(stillRunning)))
					jobs.toSeq
			}
			
			val newPopulation = results.flatten
			if(newPopulation.size == particles) Some(newPopulation)
			else None
		}
		
		@tailrec
		def refine(population: Seq[Particle[P]], numAttempts: Int, tolerance: Double, lastGoodTolerance: Double, decentFactor: Double): Seq[Particle[P]] = {
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
							case Some(w) => w(newPop.map{_.value}, tolerance) 
							case _ =>
						}
						refine(newPop, numAttempts - 1, tolerance * decentFactor, tolerance, decentFactor)
					}
				}
			}
		}
		
		val result = refine(uniformlyWeightedParticles, refinementAttempts, startTolerance, startTolerance, 0.5)
		result.map(_.value)
	}
}
