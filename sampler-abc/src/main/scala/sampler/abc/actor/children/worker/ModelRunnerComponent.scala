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

package sampler.abc.actor.children.worker

import sampler.abc.{Model, Scored}
import sampler.abc.actor.root.ScoredParticles
import sampler.abc.actor.children.GenerateParticlesFrom
import sampler.io.Logging
import sampler.maths.Random

import scala.annotation.tailrec
import scala.util.Try

class DetectedAbortionException() extends Exception("DetectedAbortionException")

class MaxRetryException(message: String = null, cause: Throwable = null)
	extends RuntimeException(message, cause)

trait ModelRunnerComponentImpl[P] extends ModelRunnerComponent[P] {
	self: AborterComponent =>
	val modelRunner = new ModelRunner {}
}

trait ModelRunnerComponent[P] {
	self: AborterComponent =>

	val model: Model[P]
	val modelRunner: ModelRunner
	val random: Random

	trait ModelRunner extends Logging {
		import model._

		def run(job: GenerateParticlesFrom[P]): Try[ScoredParticles[P]] = Try {
		  val maxParticleRetries = job.config.maxParticleRetries
		  val numReplicates = job.config.numReplicates
		  val particleChunkSize = job.config.particleChunkSize
		  val proposalDistribution = job.prevGen.proposalDistribution(model)

			@tailrec
			def getScoredParameter(failures: Int = 0): Scored[P] = {
				aborter.checkIfAborted()
				if (failures >= maxParticleRetries) throw new MaxRetryException(s"Aborted after $failures failed particle draws from previous population")
				else {
					def getScores(params: P): IndexedSeq[Double] = {
						val modelWithMetric = model.distanceToObservations(params)
						(1 to numReplicates).map(_ => modelWithMetric.sample(random))
					}

					val res: Option[Scored[P]] = for {
						params <- Some(proposalDistribution.sample(random)) if prior.density(params) > 0
						fitScores <- Some(getScores(params))
					} yield Scored(params, fitScores)

					res match {
						case Some(p) => p
						case None => getScoredParameter(failures + 1)
					}
				}
			}

			val seq = (1 to particleChunkSize).map(i => getScoredParameter())
			ScoredParticles(seq)
		}
	}
}
