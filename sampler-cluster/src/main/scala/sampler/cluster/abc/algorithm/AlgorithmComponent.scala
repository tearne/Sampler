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
import sampler.cluster.abc.actor.TaggedScoreSeq
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

trait AlgorithmComponent {
	val algorithm: Algorithm
}

trait Algorithm {
	def numberAccumulated(gen: Generation[_]): Int
	def buildReport[P](gen: Generation[P], config: ABCConfig, finalReport: Boolean): Report[P]
	def flushGeneration[P](gen: Generation[P], numParticles: Int): Generation[P]
	def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): 
		Option[TaggedScoreSeq[P]]
	def add[P](
			gen: Generation[P],
			taggedAndScoredParamSets: Seq[Tagged[Scored[P]]],
			abcParameters: ABCConfig
		): Generation[P]
}


/*
 * Use of a base trait and Impl allows us to strip out all the 
 * self typing and simplify mocking
 */
trait AlgorithmComponentImpl extends AlgorithmComponent{
	this: ToleranceComponent 
		with WeigherComponent
		with StatisticsComponent
		with Actor
		with ActorLogging
		with GettersComponent =>
	
	val algorithm: Algorithm
	
	trait AlgorithmImpl extends Algorithm {
		implicit val r = Random
		
		def weightsTable[G <: Generation[_]](gen: G) = gen.prevWeightsTable
		def numberAccumulated(gen: Generation[_]) = gen.particleInBox.size
		
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
		
		def flushGeneration[P](gen: Generation[P], numParticles: Int): Generation[P] = {
			import gen._
			assert(numParticles <= particleInBox.size)
			val seqWeighted = particleInBox.toSeq.map(_.value)
			val newTolerance = toleranceCalculator(seqWeighted, currentTolerance)
			
			val newGeneration = gen.copy(
				particleInBox = Set.empty[Tagged[Weighted[P]]],
				currentTolerance = newTolerance,
				currentIteration = currentIteration + 1,
				prevWeightsTable = weigher.consolidateToWeightsTable(model, seqWeighted)
			)
			
			newGeneration
		}
		
		//TODO can we simplify tagged and scored parm sets?
		def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): Option[TaggedScoreSeq[P]] = {
			import gen._
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
				gen: Generation[P],
				taggedAndScoredParamSets: Seq[Tagged[Scored[P]]],
				abcParameters: ABCConfig
		): Generation[P] = {
			import gen._
			
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
			
			log.info(s"+ ${taggedAndScoredParamSets.size} => ${weighedAndTagged.size} = ${newInBox.size}/${abcParameters.job.numParticles}")
			
			val newIdsObserved = {
				val union = idsObserved ++ weighedAndTagged.map(_.id)
				val memorySize = abcParameters.cluster.particleMemoryGenerations * abcParameters.job.numParticles
				if(union.size > 1.5 * memorySize) union.drop(union.size - memorySize)
				else union
			}
			
			gen.copy(
					particleInBox = newInBox,
					idsObserved = newIdsObserved
			)
		}
	}
}