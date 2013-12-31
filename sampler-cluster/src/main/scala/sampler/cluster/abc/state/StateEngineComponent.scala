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

import scala.Option.option2Iterable
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import sampler.Implicits.SamplableMap
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.state.component.ToleranceCalculatorComponent
import sampler.cluster.abc.state.component.WeigherComponent
import sampler.math.Random
import sampler.math.StatisticsComponent
import sampler.cluster.abc.Model

trait StateEngineComponent {
	val stateEngine: StateEngine
}

trait StateEngine {
	def numberAccumulated(state: State[_]): Int
	def setClient[P](state: State[P], client: ActorRef): State[P]
	def flushGeneration[P](state: State[P], numParticles: Int): State[P]
	def getMixPayload[P](state: State[P], abcParameters: ABCConfig): 
		Option[TaggedScoreSeq[P]]
	def add[P](
			state: State[P],
			abcParameters: ABCConfig,
			taggedAndScoredParamSets: Seq[Tagged[Scored[P]]],
			sender: ActorRef
		): State[P]
}


/*
 * Use of a base trait and Impl allows us to strip out all the 
 * self typing and simplify mocking
 */
trait StateEngineComponentImpl extends StateEngineComponent{
	this: ToleranceCalculatorComponent 
		with WeigherComponent
		with StatisticsComponent
		with Actor
		with ActorLogging =>
	
	val stateEngine: StateEngine
	
	trait StateEngineImpl extends StateEngine {
		implicit val r = Random
		
		def weightsTable[S <: State[_]](state: S) = state.prevWeightsTable
		def numberAccumulated(state: State[_]) = state.particleInBox.size
		
		def setClient[P](state: State[P], client: ActorRef): State[P] = {
			state.copy(client = Some(client))
		}
		
		def flushGeneration[P](state: State[P], numParticles: Int): State[P] = {
			import state._
			assert(numParticles <= particleInBox.size)
			val seqWeighted = particleInBox.toSeq.map(_.value)
			val newTolerance = toleranceCalculator(seqWeighted, currentTolerance)
			
			val newState = state.copy(
				particleInBox = Set.empty[Tagged[Weighted[P]]],
				currentTolerance = newTolerance,
				currentIteration = currentIteration + 1,
				prevWeightsTable = weigher.consolidateToWeightsTable(model, seqWeighted)
			)
			
			newState
		}
		
		//TODO can we simplify tagged and scored parm sets?
		def getMixPayload[P](state: State[P], abcParameters: ABCConfig): Option[TaggedScoreSeq[P]] = {
			import state._
			if(particleInBox.size > 0)
				Some(TaggedScoreSeq(particleInBox
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
		
		def add[P](
				state: State[P],
				abcParameters: ABCConfig,
				taggedAndScoredParamSets: Seq[Tagged[Scored[P]]],
				sender: ActorRef
		): State[P] = {
			import state._
			
			type T = Tagged[Weighted[P]]
			
			val weighedAndTagged: Seq[T] = {
				val t:Seq[Option[T]] = taggedAndScoredParamSets
					.filter(tagged => !idsObserved.contains(tagged.id))
					.map{case Tagged(scored, id) =>
						weigher.getWeightOption(
								model, 
								scored, 
								prevWeightsTable, 
								currentTolerance
						).map{weighted => Tagged(weighted, id)}
					}
				
				t.flatten
			}
			
			val newInBox = particleInBox ++ weighedAndTagged
			
			log.info(s"+ ${taggedAndScoredParamSets.size} => ${weighedAndTagged.size} = ${newInBox.size}/${abcParameters.job.numParticles} (${sender.path})")
			
			val newIdsObserved = {
				val union = idsObserved ++ weighedAndTagged.map(_.id)
				val memorySize = abcParameters.cluster.particleMemoryGenerations * abcParameters.job.numParticles
				if(union.size > 1.5 * memorySize) union.drop(union.size - memorySize)
				else union
			}
			
			state.copy(
					particleInBox = newInBox,
					idsObserved = newIdsObserved
			)
		}
	}
}