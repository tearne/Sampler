/*
 * Copyright (c) 2012-13 Crown Copyright 
 *                       Animal Health and Veterinary Laboratories Agency
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

package sampler.cluster.abc.distributed

import sampler.abc.ABCModel
import sampler.math.Random
import sampler.run.WrappedAborter
import scala.util.Try
import sampler.cluster.abc.ABCJob
import java.util.concurrent.atomic.AtomicBoolean
import sampler.abc.builder.ParticleBuilderComponent
import sampler.data.Distribution
import sampler.math.Partition
import scala.annotation.tailrec
import sampler.run.DetectedAbortionException
import sampler.data.SerialSampler
import sampler.io.Logging
import sampler.abc.MaxRetryException

trait ModelRunner extends Logging{
	val model: ABCModel
	import model._
	implicit val random: Random
	
	val aborted: AtomicBoolean = new AtomicBoolean(false)

	def abort() { aborted.set(true) }
	def isAborted = aborted.get
	def reset() { aborted.set(false) }
	
	def run(job: ABCJob[_]): Try[Seq[ScoredParam]] = Try{
		val prevPopulation = job.population.asInstanceOf[model.Population]
		
		val weightsTable = prevPopulation.groupBy(_.value).map{case (k,v) => (k, v.map(_.weight).sum)}.toIndexedSeq
		val (parameters, weights) = weightsTable.unzip
		val samplablePopulation = Distribution.fromPartition(parameters, Partition.fromWeights(weights))
		
		@tailrec
		def nextParticle(failures: Int = 0): ScoredParam = {
			if(isAborted) throw new DetectedAbortionException("Abort flag was set")
			else if(failures >= job.abcParams.particleRetries) throw new MaxRetryException(s"Aborted after the maximum of $failures trials")
			else{
				def getScores(params: Parameters): IndexedSeq[Double] = {
					val modelWithMetric = modelDistribution(params).map(_.distanceToObserved)
					SerialSampler(modelWithMetric)(_.size == job.abcParams.reps)
				}
				
				val res: Option[ScoredParam] = for{
					params <- Some(samplablePopulation.sample().perturb()) if prior.density(params) > 0
					fitScores <- Some(getScores(params))
				} yield ScoredParam(params, fitScores)
				
				res match {
					case Some(p) => p
					case None => nextParticle(failures + 1)
				}
			}
		}
		
		val r = (1 to job.abcParams.particleChunking).map(i => nextParticle()) 
		log.info(s"${job.abcParams.particleChunking} requested, ${r.size} are being returned")
		r
	}
}

