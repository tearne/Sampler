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

package sampler.cluster.abc.state

import sampler.abc.ABCModel
import sampler.cluster.abc.parameters.ABCParameters
import sampler.cluster.abc.actor.Tagged
import sampler.abc.Weighted
import scala.collection.immutable.SortedSet

trait EncapsulatedState extends Serializable {
	type M <: ABCModel
	val model: M
	import model._
	
	val state: State[model.ParameterSet]
}
object EncapsulatedState {
	def apply[M <: ABCModel](model0 : M)(state0 : State[model0.ParameterSet]) =
		new EncapsulatedState {
			type M = model0.type	//Why do we need this when it's defined in the type bounds of apply?
			val model : M = model0
			val state = state0
		}
  	
  	def init(model: ABCModel, abcParameters: ABCParameters): EncapsulatedState = {
		EncapsulatedState(model){
			import model._
			val uniformProb = 1.0 / abcParameters.job.numParticles
			val weightsTable = (1 to abcParameters.job.numParticles)
				.par
				.map(i => prior.sample() -> uniformProb)
				.seq
				.toMap
				
			State(
				None,
				Set.empty[Tagged[Weighted[ParameterSet]]],
				SortedSet.empty[Long],
				Double.MaxValue,
				0,
				weightsTable
			)
		}
	}
}