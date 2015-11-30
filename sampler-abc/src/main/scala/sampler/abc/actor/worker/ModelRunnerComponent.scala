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

package sampler.abc.actor.worker

import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.util.Try
import sampler.abc.actor.message.Job
import sampler.data.DistributionBuilder
import sampler.data.SerialSampler
import sampler.io.Logging
import sampler.math.Random
import sampler.abc.Model
import sampler.abc.Scored
import sampler.abc.actor.Tagged
import sampler.abc.actor.message.GenerateParticlesFrom
import sampler.abc.actor.message.ScoredParticles
import sampler.data.ConvergenceProtocol
import sampler.data.MaxMetric
import sampler.data.Distribution

trait ModelRunnerComponent[P] {
	val model: Model[P]
	val modelRunner: ModelRunner
	implicit val random: Random
	
	trait ModelRunner extends Logging{
		import model._
		
		val aborted: AtomicBoolean = new AtomicBoolean(false)
		
		def abort() { aborted.set(true) }
		def reset() { aborted.set(false) }
		private def isAborted = aborted.get
		
		def run(job: GenerateParticlesFrom[P]): Try[ScoredParticles[P]] = Try{
			val maxParticleRetries = job.config.algorithm.maxParticleRetries
			val proposalDist: Distribution[P] =  job.prevGen.proposalDistribution(model, random)
			
			@tailrec
			def getScoredParameter(failures: Int = 0): Scored[P] = {
				if(isAborted) throw new DetectedAbortionException("Abort flag was set")
				else if(failures >= maxParticleRetries) throw new MaxRetryException(s"Aborted after $failures failed particle draws from previous population")
				else{
					def getScores(params: P): IndexedSeq[Double] = {
						val modelWithMetric = model.distanceToObservations(params)
						val replicates = job.config.job.numReplicates
						SerialSampler.apply(modelWithMetric)(new ConvergenceProtocol[Double](replicates, 0.5, 1000000) with MaxMetric).toIndexedSeq
					}
					
					val res: Option[Scored[P]] = for{
						params <- Some(proposalDist.sample) if prior.density(params) > 0
						fitScores <- Some(getScores(params))
					} yield Scored(params, fitScores)
					
					res match {
						case Some(p) => p
						case None => getScoredParameter(failures + 1)
					}
				}
			}
			
			val seq = (1 to job.config.algorithm.particleChunkSize).map(i => Tagged(getScoredParameter()))
			ScoredParticles(seq)
		}
	}
}
