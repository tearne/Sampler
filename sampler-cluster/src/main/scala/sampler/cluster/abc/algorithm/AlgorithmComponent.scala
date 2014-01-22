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

import scala.Option.option2Iterable
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import sampler.Implicits.SamplableMap
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.algorithm.component.ToleranceComponent
import sampler.cluster.abc.algorithm.component.WeigherComponent
import sampler.math.Random
import sampler.math.StatisticsComponent
import sampler.cluster.abc.Model
import sampler.cluster.abc.actor.Report
import sampler.data.Distribution
import sampler.cluster.abc.actor.root.Getters
import sampler.cluster.abc.actor.root.GettersComponent
import sampler.cluster.abc.actor.TaggedScoredSeq
import sampler.cluster.abc.actor.TaggedWeighedSeq

trait AlgorithmComponent {
	val algorithm: Algorithm
}

trait Algorithm {
	def addWeighted[P](incomingWeighted: TaggedWeighedSeq[P], gen: Generation[P], config: ABCConfig): Generation[P]
	def filterAndQueueForWeighing[P](taggedAndScored: TaggedScoredSeq[P], gen: Generation[P]): Generation[P]
	def flushGeneration[P](gen: Generation[P], numParticles: Int): Generation[P]
	def isEnoughParticles(gen: Generation[_], config: ABCConfig): Boolean
	def emptyWeighingBuffer[P](gen: Generation[P]): Generation[P]
	
	def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): Option[TaggedScoredSeq[P]]
	def buildReport[P](gen: Generation[P], config: ABCConfig, finalReport: Boolean): Report[P]
}


/*
 * Use of a base trait and Impl allows us to strip out all the 
 * self typing and simplify mocking
 */
trait AlgorithmComponentImpl extends AlgorithmComponent{
	this: ToleranceComponent 
		with StatisticsComponent
		with Actor
		with ActorLogging
		with GettersComponent =>
	
	val algorithm: Algorithm
	
	trait AlgorithmImpl extends Algorithm {
		implicit val r = Random
		
		def addWeighted[P](
				incoming: TaggedWeighedSeq[P],
				gen: Generation[P],
				config: ABCConfig
		): Generation[P] = {
			import gen._
			
			val newWeighted = weighted ++ incoming.seq
			
			gen.copy(
					weighted = newWeighted
			)
		}
		
		def filterAndQueueForWeighing[P](
			taggedAndScoredParamSets: TaggedScoredSeq[P],
			gen: Generation[P]
		): Generation[P] = {
			val observedIds = gen.idsObserved
			val filtered = taggedAndScoredParamSets.seq.filter(tagged => !observedIds.contains(tagged.id))
			gen.copy(
					dueWeighing = gen.dueWeighing ++ filtered,
					idsObserved = observedIds ++ filtered.map(_.id)
			)
		}
		
		def flushGeneration[P](gen: Generation[P], numParticles: Int): Generation[P] = {
			import gen._
			assert(numParticles <= weighted.size)
			val seqWeighted = weighted.toSeq.map(_.value) //Strip out tags
			val newTolerance = toleranceCalculator(seqWeighted, currentTolerance)
			
			def consolidateToWeightsTable[P](model: Model[P], population: Seq[Weighted[P]]): Map[P, Double] = {
				population
				.groupBy(_.params)
				.map{case (k,v) => (k, v.map(_.weight).sum)}
			}
			
			val newGeneration = gen.copy(
				weighted = Seq.empty[Tagged[Weighted[P]]],
				currentTolerance = newTolerance,
				currentIteration = currentIteration + 1,
				prevWeightsTable = consolidateToWeightsTable(model, seqWeighted)
			)
			
			newGeneration
		}
		
		def isEnoughParticles(gen: Generation[_], config: ABCConfig): Boolean =
			gen.weighted.size >= config.job.numParticles
		
		def emptyWeighingBuffer[P](gen: Generation[P]): Generation[P] = 
			gen.copy(dueWeighing = Seq.empty[Tagged[Scored[P]]])
			
		def weightsTable[G <: Generation[_]](gen: G) = gen.prevWeightsTable
//		def numberAccumulated(gen: Generation[_]) = gen.weighted.size
		
		//TODO can we simplify tagged and scored parm sets?
		def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): Option[TaggedScoredSeq[P]] = {
			import gen._
			
			val mixingSize = abcParameters.cluster.mixPayloadSize
			
			if(weighted.size > mixingSize) {
				val oneOfEachParticle = 
					weighted.toSeq
						.map{case Tagged(weighted, uid) =>
							Tagged(weighted.scored, uid) -> 1
						}
						.toMap
					
				val res = oneOfEachParticle.draw(mixingSize)
					._2		//TODO, this is all a bit nasty
					.keys
					.toSeq
				
				Some(TaggedScoredSeq(res))
			} else if(weighted.size > 0){
				val res = weighted
					.toSeq
					.map{case Tagged(weighted, uid) =>
						Tagged(weighted.scored, uid)
					}
				Some(TaggedScoredSeq(res))
			} else None
		}
			
		def buildReport[P](gen: Generation[P], config: ABCConfig, finalReport: Boolean): Report[P] = {
			val samples: Seq[P] = Distribution
				.fromProbabilityTable(gen.prevWeightsTable)
				.until(_.size == getters.getNumParticles(config))
				.sample
			
			Report(
					gen.currentIteration,
					gen.currentTolerance,
					samples,
					finalReport
			)
		}
	}
}