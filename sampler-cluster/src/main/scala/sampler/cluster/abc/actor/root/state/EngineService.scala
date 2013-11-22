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

package sampler.cluster.abc.actor.root.state

import sampler.abc.ABCModel
import akka.actor.ActorRef
import scala.collection.immutable.SortedSet
import sampler.abc.Weighted
import sampler.io.Logging
import sampler.abc.Scored
import sampler.math.StatisticsComponent
import sampler.cluster.abc.actor.root.Tagged
import scala.Option.option2Iterable
import sampler.cluster.abc.actor.TaggedAndScoredParameterSets
import sampler.abc.ABCParameters
import sampler.cluster.abc.actor.root.State
import sampler.cluster.abc.actor.root.EncapsulatedState
import sampler.abc.ABCParameters

trait StateEngineService 
	extends StateEngineComponent 
	with ToleranceCalculatorComponent 
	with WeigherComponent 
	with StatisticsComponent 
	with Logging

trait StateEngineComponent{
	self: WeigherComponent
		with ToleranceCalculatorComponent
		with Logging =>
	
	val numGenerationsMemory: Int
	
	def init(model: ABCModel, abcParameters: ABCParameters): EncapsulatedState = {
		EncapsulatedState(model){
			import model._
			val generationZero = (1 to abcParameters.numParticles)
				.par
				.map(i => 
					Weighted(Scored(prior.sample(), Nil), 1.0)
				)
				.seq
				
			val weightsTable = generationZero
				.map{weighed => (weighed.value, weighed.weight)}
				.toMap
				
			State(
				null,
				Set.empty[Tagged[Weighted[ParameterSet]]],
				SortedSet.empty[Long],
				Double.MaxValue,
				0,
				weightsTable,
				Nil
			)
		}
	}
	
	def numberAccumulated(eState: EncapsulatedState) = eState.state.inBox.size
	
	def setClient(eState: EncapsulatedState, client: ActorRef) = {
		EncapsulatedState(eState.model){
			eState.state.copy(client = client)
		}
	}
	
	def flushGeneration(eState: EncapsulatedState, numParticles: Int): EncapsulatedState = {
		import eState.state._
		assert(numParticles <= inBox.size)
		val seqWeighted = inBox.toSeq.map(_.value)
		val newTolerance = toleranceCalculator(seqWeighted, currentTolerance / 2)
		log.info("New tolerance: {}", newTolerance)
		
		val newState = copy(
			inBox = Set.empty[Tagged[Weighted[eState.model.ParameterSet]]],
			currentTolerance = newTolerance,
			currentIteration = currentIteration + 1,
			weightsTable = weigher.consolidateToWeightsTable(eState.model)(seqWeighted),
			weightedParameterSets = seqWeighted
		)
		
		EncapsulatedState(eState.model)(newState)
	}
	
	def getMixPayload(eState: EncapsulatedState): Option[TaggedAndScoredParameterSets[Scored[eState.model.ParameterSet]]] = {
		import eState.state._
		if(inBox.size > 0)
			Some(TaggedAndScoredParameterSets(inBox.toSeq.map{case Tagged(weighted, origin) =>
					Tagged(weighted.scored, origin)
			}))
		else None
	}
	
	def add(
			eState: EncapsulatedState
	)(
			abcParameters: ABCParameters,
			taggedAndScoredParamSets: Seq[Tagged[Scored[eState.model.ParameterSet]]], 
			sender: ActorRef
	): EncapsulatedState = {
		import eState.state._
		type T = Tagged[Weighted[eState.model.ParameterSet]]
		val weighedAndTagged = {
			val t:Seq[Option[T]] = taggedAndScoredParamSets
				.filter(tagged => !idsObserved.contains(tagged.id))
				.map{case Tagged(scored, address) =>
					val cast: Scored[eState.model.ParameterSet] = scored.asInstanceOf[Scored[eState.model.ParameterSet]]
					weigher
						.filterAndWeighScoredParameterSet(eState.model)(cast, weightsTable, currentTolerance)
						.map{weighted => Tagged(weighted, System.currentTimeMillis())}
				}
			
			t.flatten
		}
		
		val newInBox = inBox ++ weighedAndTagged
		
		log.info(
				"Received {} samples, kept {}, accumulated {}: sender:{}", 
				taggedAndScoredParamSets.size.toString, 
				weighedAndTagged.size.toString, 
				newInBox.size.toString,
				sender
		)
		
		val newIdsObserved = {
			val union = idsObserved ++ weighedAndTagged.map(_.id)
			val memorySize = numGenerationsMemory * abcParameters.numParticles
			if(union.size > 1.5 * memorySize) union.drop(union.size - memorySize)
			else union
		}
		
		EncapsulatedState(eState.model)(
			copy(
				inBox = newInBox,
				idsObserved = newIdsObserved
			)
		)
	}
}