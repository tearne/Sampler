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
import sampler.cluster.abc.actor.root.State
import sampler.cluster.abc.actor.root.EncapsulatedState
import sampler.cluster.abc.parameters.ABCParameters
import sampler.Implicits._
import sampler.math.Random

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
	
	implicit val r = Random
			
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
				null,
				Set.empty[Tagged[Weighted[ParameterSet]]],
				SortedSet.empty[Long],
				Double.MaxValue,
				0,
				weightsTable
			)
		}
	}
	
	def numberAccumulated(eState: EncapsulatedState) = eState.state.particleInBox.size
	
	def setClient(eState: EncapsulatedState, client: ActorRef) = {
		EncapsulatedState(eState.model){
			eState.state.copy(client = client)
		}
	}
	
	def flushGeneration(eState: EncapsulatedState, numParticles: Int): EncapsulatedState = {
		import eState.state._
		assert(numParticles <= particleInBox.size)
		val seqWeighted = particleInBox.toSeq.map(_.value)
		val newTolerance = toleranceCalculator(seqWeighted, currentTolerance / 2)
		log.info("New tolerance: {}", newTolerance)
		
		val newState = copy(
			particleInBox = Set.empty[Tagged[Weighted[eState.model.ParameterSet]]],
			currentTolerance = newTolerance,
			currentIteration = currentIteration + 1,
			prevWeightsTable = weigher.consolidateToWeightsTable(eState.model)(seqWeighted)//,
			//prevWeighedParticles = seqWeighted
		)
		
		EncapsulatedState(eState.model)(newState)
	}
	
	def getMixPayload(eState: EncapsulatedState, abcParameters: ABCParameters): Option[TaggedAndScoredParameterSets[Scored[eState.model.ParameterSet]]] = {
		import eState.state._
		if(particleInBox.size > 0)
			Some(TaggedAndScoredParameterSets(particleInBox
				.toSeq
				.map{case Tagged(weighted, uid) =>
					Tagged(weighted.scored, uid) -> 1
				}
				.toMap
				.draw(math.min(particleInBox.size, abcParameters.cluster.mixPayloadSize))
				._2		//TODO, this is all a bit nasty
				.keys
				.toSeq
			))
		else None
	}
	
	def add(
			eState: EncapsulatedState,
			abcParameters: ABCParameters,
			sender: ActorRef
	)(
			taggedAndScoredParamSets: Seq[Tagged[Scored[eState.model.ParameterSet]]]
	): EncapsulatedState = {
		import eState.state._
		
		type T = Tagged[Weighted[eState.model.ParameterSet]]
		
		//TODO 
		/*
		 * It's possible that the same parameters are sampled twice,
		 * leading to add being called on two separate occasions, and 
		 * weighted parameters being calculated separately, rather than
		 * together.  
		 * 
		 * Can we just aggregate scored (& tagged) parameters, leaving 
		 * weights calculation until the generation is finalised? 
		 */ 
		
		val weighedAndTagged: Seq[T] = {
			val t:Seq[Option[T]] = taggedAndScoredParamSets
				.filter(tagged => !idsObserved.contains(tagged.id))
				.map{case Tagged(scored, id) =>
					val cast: Scored[eState.model.ParameterSet] = scored.asInstanceOf[Scored[eState.model.ParameterSet]]
					weigher
						.weighScoredParticle(eState.model)(cast, prevWeightsTable, currentTolerance)
						.map{weighted => Tagged(weighted, id)}
				}
			
			t.flatten
		}
		
		val newInBox = particleInBox ++ weighedAndTagged
		
		log.info(
				"+ {} => {} = {}/{} ({})", 
				taggedAndScoredParamSets.size.toString, 
				weighedAndTagged.size.toString, 
				newInBox.size.toString,
				abcParameters.job.numParticles.toString,
				sender.path
		)
		
		val newIdsObserved = {
			val union = idsObserved ++ weighedAndTagged.map(_.id)
			val memorySize = abcParameters.cluster.particleMemoryGenerations * abcParameters.job.numParticles
			if(union.size > 1.5 * memorySize) union.drop(union.size - memorySize)
			else union
		}
		
		EncapsulatedState(eState.model)(
			copy(
				particleInBox = newInBox,
				idsObserved = newIdsObserved
			)
		)
	}
}