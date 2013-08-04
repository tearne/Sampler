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

package sampler.abc.population

import scala.annotation.tailrec
import sampler.data.Empirical._
import sampler.run.UserInitiatedAbortException
import sampler.data.SerialSampleBuilder
import sampler.abc._
import sampler.run.local.Aborter
import sampler.data.Samplable
import sampler.math.Random
import sampler.data.EmpiricalWeighted

object Population {
	def apply(model: ABCModel)(
			prevPopulation: model.Population,
			quantity: Int, 
			tolerance: Double,
			aborter: Aborter,
			meta: ABCParameters,
			random: Random
	): model.Population = {
		import model._
		implicit val r = random
		
		val empiricalPopulation: EmpiricalWeighted[Parameters] = prevPopulation.groupBy(_.value).map{case (k,v) => (k,v.map(_.weight).sum)}.toEmpiricalWeighted
		val samplablePopulation = empiricalPopulation.toSamplable
		
		@tailrec
		def nextParticle(failures: Int = 0): Particle[Parameters] = {
			if(aborter.isAborted) throw new UserInitiatedAbortException("Abort flag was set")
			else if(failures >= meta.particleRetries) throw new RefinementAbortedException(s"Aborted after the maximum of $failures trials")
			else{
				def getScores(params: Parameters): IndexedSeq[Double] = {
					val modelWithMetric = samplableModel(params, observations).map(_.distanceTo(observations))
					val modelWithScores = SerialSampleBuilder(modelWithMetric)(_.size == meta.reps)
						.filter(_ <= tolerance)
					modelWithScores
				}
				
				def getWeight(params: Parameters, numPassed: Int): Option[Double] = {
					val fHat = numPassed.toDouble / meta.reps
					val numerator = fHat * prior.density(params)
					val denominator = empiricalPopulation.probabilityTable.map{case (params0, probability) => 
						probability.value * params0.perturbDensity(params)
					}.sum
					if(numerator > 0 && denominator > 0) Some(numerator / denominator)
					else None	
				}
				
				val res: Option[Particle[Parameters]] = for{
					params <- Some(samplablePopulation.sample().perturb()) if prior.density(params) > 0
					fitScores <- Some(getScores(params))
					weight <- getWeight(params, fitScores.size) 
				} yield(Particle(params, weight, fitScores.min))
				
				res match {
					case Some(p: Particle[Parameters]) => p
					case None => nextParticle(failures + 1)
				}
			}
		}
		
		(1 to quantity).map(i => nextParticle()) 
	}
}