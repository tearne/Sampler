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

package sampler.cluster.abc.distributed.util

import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.util.Try

import sampler.abc.MaxRetryException
import sampler.cluster.abc.distributed.ABCModel
import sampler.cluster.abc.distributed.actor.Messages.Job
import sampler.data.Distribution
import sampler.data.SerialSampler
import sampler.io.Logging
import sampler.math.Partition
import sampler.math.Random
import sampler.run.DetectedAbortionException

trait AbortableModelRunner extends Logging{
	val model: ABCModel
	import model._
	implicit val random: Random
	
	val aborted: AtomicBoolean = new AtomicBoolean(false)

	def abort() { aborted.set(true) }
	def isAborted = aborted.get
	def reset() { aborted.set(false) }
	
	def run(job: Job): Try[Seq[Scored]] = Try{
		val prevPopulation = job.population.asInstanceOf[Seq[Weighted]]
		val weightsTable = prevPopulation.groupBy(_.parameterSet).map{case (k,v) => (k, v.map(_.weight).sum)}.toIndexedSeq
		val (parameterSets, weights) = weightsTable.unzip
		val samplablePopulation = Distribution.fromPartition(parameterSets, Partition.fromWeights(weights))
		
		@tailrec
		def getScoredParameter(failures: Int = 0): Scored = {
			if(isAborted) throw new DetectedAbortionException("Abort flag was set")
			else if(failures >= job.abcParams.particleRetries) throw new MaxRetryException(s"Aborted after the maximum of $failures trials")
			else{
				def getScores(params: ParameterSet): IndexedSeq[Double] = {
					val modelWithMetric = modelDistribution(params).map(_.distanceToObserved)
					//TODO in parallel?
					SerialSampler(modelWithMetric)(_.size == job.abcParams.numReplicates)
				}
				
				val res: Option[Scored] = for{
					params <- Some(samplablePopulation.sample().perturb()) if prior.density(params) > 0
					fitScores <- Some(getScores(params))
				} yield Scored(params, fitScores)
				
				res match {
					case Some(p) => p
					case None => getScoredParameter(failures + 1)
				}
			}
		}
		
		(1 to job.abcParams.particleChunkSize).map(i => getScoredParameter()) 
	}
}

object AbortableModelRunner{
	def apply(model0: ABCModel) = new AbortableModelRunner{
		val model = model0
		val random = Random
	}
}

