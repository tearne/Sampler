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

package sampler.cluster.abc.actor.root
import sampler.abc.ABCModel
import scala.collection.SortedSet
import sampler.abc.ABCParameters
import akka.actor.ActorLogging
import akka.actor.ActorRef
import sampler.cluster.abc.actor.root.ToleranceCalculatorComponent
import sampler.cluster.abc.actor.root.WeigherComponent
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedAndScoredParameterSets
import scala.Option.option2Iterable

trait ModelComponent{
	val model: ABCModel
}

trait RootStateComponent{
	self: WeigherComponent
		with ModelComponent
		with ToleranceCalculatorComponent
		with ActorLogging =>
	import model._
		
	val rootStateInitialiser: RootStateInitialiser
	
	trait RootStateInitialiser{
		def apply(abcParameters: ABCParameters, client: ActorRef): RootState = {
			val inBox = Set.empty[Tagged[Weighted]]
			val idsObserved = SortedSet.empty[Long]
			val currentTolerance = Double.MaxValue
			val currentIteration = 0
							
			val generationZero = (1 to abcParameters.numParticles)
				.par
				.map(i => 
					Weighted(Scored(model.prior.sample(), Nil), 1.0)
				)
				.seq
									
			val currentWeightsTable = generationZero
				.map{weighed => (weighed.parameterSet, weighed.weight)}
				.toMap
			
			RootStateImpl(
				client,
				inBox,
				idsObserved,
				currentTolerance,
				currentIteration,
				currentWeightsTable,
				generationZero
			)
		}
	}
	
	trait RootState{
		val client: ActorRef
		
		def getMixPayload: Option[TaggedAndScoredParameterSets[Scored]]
		def pruneObservedIds(maxSize: Int): RootState
		
		def add(taggedAndScoredParamSets: Seq[Tagged[Scored]], sender: ActorRef): RootState
		
		def numberAccumulated: Int
		def currentIteration: Int
		
		def flushGeneration(numParticles: Int): RootState
		val weightedParameterSets: Seq[Weighted]
	}
	
	case class RootStateImpl(
		client: ActorRef,
		inBox: Set[Tagged[Weighted]],
		idsObserved: SortedSet[Long],
		currentTolerance: Double,
		currentIteration: Int,
		weightsTable: Map[ParameterSet, Double],
		weightedParameterSets: Seq[Weighted] = Nil
	) extends RootState {
		def numberAccumulated = inBox.size
		
		def flushGeneration(numParticles: Int): RootState = {
			assert(numParticles <= inBox.size)
			val seqWeighted = inBox.toSeq.map(_.value)
			val newTolerance = toleranceCalculator(seqWeighted, currentTolerance / 2)
			log.info("New tolerance: {}", newTolerance)
			
			copy(
				inBox = Set.empty,
				currentTolerance = newTolerance,
				currentIteration = currentIteration + 1,
				weightsTable = weigher.consolidateToWeightsTable(model)(seqWeighted),
				weightedParameterSets = seqWeighted
			)
		}
		
		def getMixPayload(): Option[TaggedAndScoredParameterSets[Scored]] = {
			if(inBox.size > 0)
				Some(TaggedAndScoredParameterSets(inBox.toSeq.map{case Tagged(weighted, origin) =>
						Tagged(weighted.scored, origin)
				}))
			else None
		}
		
		def pruneObservedIds(maxSize: Int): RootState = {
			if(idsObserved.size > 10 * maxSize)
				copy(idsObserved = this.idsObserved.drop(maxSize))
			else this
		}
		
		def add(taggedAndScoredParamSets: Seq[Tagged[Scored]], sender: ActorRef): RootState = {
			val weighedAndTagged = taggedAndScoredParamSets
				.filter(tagged => !idsObserved.contains(tagged.id))
				.map{case Tagged(scored, address) =>
					weigher
						.filterAndWeighScoredParameterSet(model)(scored, weightsTable, currentTolerance)
						.map{weighted => Tagged(weighted, System.currentTimeMillis())}
					}.flatten
			
			val newInBox = inBox ++ weighedAndTagged
			val newIdsObserved = idsObserved ++ weighedAndTagged.map(_.id)
			
			log.info(
					"Received {} samples, kept {}, accumulated {}: sender:{}", 
					taggedAndScoredParamSets.size.toString, 
					weighedAndTagged.size.toString, 
					inBox.size.toString,
					sender
			)
			
			copy(
				inBox = newInBox,
				idsObserved = newIdsObserved
			)
		}
	}
}
