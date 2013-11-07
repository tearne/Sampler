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

package sampler.abc.builder
import sampler.abc.ABCModel
import sampler.abc.ABCParameters
import sampler.math.Random
import sampler.run.Aborter
import sampler.data.Distribution
import sampler.math.Partition
import sampler.run.DetectedAbortionException
import sampler.abc.MaxRetryException
import scala.annotation.tailrec
import sampler.data.SerialSampler
import sampler.abc.EncapsulatedPopulation

trait ParticleBuilderComponent{
	val particleBuilder: ParticleBuilder
	
	/*
	 *  Builds a new seq of particles by applying ABC sampling and weighting based 
	 *  on the previous population.  Note that the size of the outgoing 
	 *  population need not be the same as the incoming.  This is used when
	 *  load of discovering particles is spread across different threads.
	 */
	class ParticleBuilder {
		def apply(model: ABCModel)(
				prevPopulation: Seq[model.Weighted],
				numParticles: Int, 
				tolerance: Double,
				aborter: Aborter,
				meta: ABCParameters,
				random: Random
		): Seq[model.Weighted] = {
			import model._
			implicit val r = random
			
			val weightsTable = prevPopulation.groupBy(_.parameterSet).map{case (k,v) => (k, v.map(_.weight).sum)}.toIndexedSeq
			val (parameters, weights) = weightsTable.unzip
			val samplablePopulation = Distribution.fromPartition(parameters, Partition.fromWeights(weights))
			
			@tailrec
			def nextParticle(failures: Int = 0): Weighted = {
				if(aborter.isAborted) throw new DetectedAbortionException("Abort flag was set")
				else if(failures >= meta.particleRetries) throw new MaxRetryException(s"Aborted after the maximum of $failures trials")
				else{
					def getScores(params: ParameterSet): IndexedSeq[Double] = {
						val modelWithMetric = modelDistribution(params).map(_.distanceToObserved)
						SerialSampler(modelWithMetric)(_.size == meta.numReplicates)
					}
					
					def getWeight(params: ParameterSet, scores: IndexedSeq[Double]): Option[Double] = {
						val fHat = scores.filter(_ < tolerance).size.toDouble
						val numerator = fHat * prior.density(params)
						val denominator = weightsTable.map{case (params0, weight) => 
							weight * params0.perturbDensity(params)
						}.sum
						if(numerator > 0 && denominator > 0) Some(numerator / denominator)
						else None
					}
					
					val res: Option[Weighted] = for{
						params <- Some(samplablePopulation.sample().perturb()) if prior.density(params) > 0
						fitScores <- Some(getScores(params))
						weight <- getWeight(params, fitScores) 
//						meanFit = fitScores.sum.toDouble / fitScores.size
					} yield Weighted(Scored(params, fitScores), weight)
					
					res match {
						case Some(p: Weighted) => p
						case None => 
							nextParticle(failures + 1)
					}
				}
			}
			
			(1 to numParticles).map(i => nextParticle()) 
		}
	}
}