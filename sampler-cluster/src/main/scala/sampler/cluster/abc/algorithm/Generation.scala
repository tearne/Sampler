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

package sampler.cluster.abc.algorithm

import akka.actor.ActorRef
import scala.collection.immutable.SortedSet
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.Model
import sampler.cluster.abc.config.ABCConfig

case class Generation[P](
	model: Model[P],
	particleInBox: Set[Tagged[Weighted[P]]],
	idsObserved: SortedSet[Long],
	currentTolerance: Double,
	currentIteration: Int,
	prevWeightsTable: Map[P, Double]
)

object Generation {
	def init[P](model: Model[P], abcParameters: ABCConfig): Generation[P] = {
		val uniformProb = 1.0 / abcParameters.job.numParticles
		val weightsTable = (1 to abcParameters.job.numParticles)
			.par
			.map(i => model.prior.sample() -> uniformProb)
			.seq
			.toMap
			
		Generation(
			model,
			Set.empty[Tagged[Weighted[P]]],
			SortedSet.empty[Long],
			Double.MaxValue,
			0,
			weightsTable
		)
	}
}