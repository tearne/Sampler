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
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.WeighedParticles
import sampler.cluster.abc.actor.LoggingAdapterComponent

trait AlgorithmComponent {
	val algorithm: Algorithm
}

trait Algorithm {
	def addWeighted[P](incomingWeighted: WeighedParticles[P], gen: Generation[P]): Generation[P]
	def filterAndQueueForWeighing[P](taggedAndScored: ScoredParticles[P], gen: Generation[P]): Generation[P]
	def flushGeneration[P](gen: Generation[P], numParticles: Int): Generation[P]
	def isEnoughParticles(gen: Generation[_], config: ABCConfig): Boolean
	def emptyWeighingBuffer[P](gen: Generation[P]): Generation[P]
	
	def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): Option[ScoredParticles[P]]
	def buildReport[P](gen: Generation[P], config: ABCConfig): Report[P]
}


/*
 * Use of a base trait and Impl allows us to strip out all the 
 * self typing and simplify mocking
 */
trait AlgorithmComponentImpl extends AlgorithmComponent {
	this: ToleranceComponent 
		with StatisticsComponent
		with LoggingAdapterComponent
//		with Actor		// TODO think about whether this needs to be here
//		with ActorLogging
		with GettersComponent =>
	
	val algorithm: Algorithm
	
	trait AlgorithmImpl extends Algorithm {
		implicit val r = Random
		
		def addWeighted[P](
				incoming: WeighedParticles[P],
				gen: Generation[P]
		): Generation[P] = {
			val weightedParticles = gen.weighted
			
			val newWeighted = weightedParticles ++ incoming.seq
			
			gen.copy(
					weighted = newWeighted
			)
		}
		
		def filterAndQueueForWeighing[P](
			taggedAndScoredParamSets: ScoredParticles[P],
			gen: Generation[P]
		): Generation[P] = {
			val observedIds = gen.idsObserved
			val particlesDueWeighting = gen.dueWeighing
			
			val filtered = taggedAndScoredParamSets.seq.filter(tagged => !observedIds.contains(tagged.id))
			
			gen.copy(
					dueWeighing = particlesDueWeighting ++ filtered,
					idsObserved = observedIds ++ filtered.map(_.id)
			)
		}
		
		def flushGeneration[P](gen: Generation[P], numParticles: Int): Generation[P] = {
			val weightedParticles = gen.weighted
			val currentTolerance = gen.currentTolerance
			val currentIteration = gen.currentIteration
			val model = gen.model
			
			assert(numParticles <= weightedParticles.size)
			val seqWeighted = weightedParticles.toSeq.map(_.value) //Strip out tags
			val newTolerance = toleranceCalculator(seqWeighted, currentTolerance)
			
			def consolidateToWeightsTable[P](model: Model[P], population: Seq[Weighted[P]]): Map[P, Double] = {
				population
				.groupBy(_.params)
				.map{case (k,v) => (k, v.map(_.weight).sum)}
			}
			
			val newGeneration = gen.copy(
			    dueWeighing = Seq.empty[Tagged[Scored[P]]],
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
			
			// TODO this method loses type of key - not actually used. Delete??
		def weightsTable[G <: Generation[_]](gen: G) = gen.prevWeightsTable
		
		//TODO can we simplify tagged and scored parm sets?
		//TODO discuss this method
		//TODO testing difficult because of random drawing
		def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): Option[ScoredParticles[P]] = {
			val weightedParticles = gen.weighted
			
			val mixingSize = abcParameters.cluster.mixPayloadSize
			
			if(weightedParticles.size > mixingSize) {
				val oneOfEachParticle = 
					weightedParticles.toSeq
						.map{case Tagged(weighted, uid) =>
							Tagged(weighted.scored, uid) -> 1
						}
						.toMap
					
				val res = oneOfEachParticle.draw(mixingSize)
					._2		//TODO, this is all a bit nasty
					.keys
					.toSeq
				
				if(res.size == 999) logg.warning("AAAAAAAAAAAAAAAAAAAA")
					
				Some(ScoredParticles(res))
			} else if(weightedParticles.size > 0){
				val res = weightedParticles
					.toSeq
					.map{case Tagged(weighted, uid) =>
						Tagged(weighted.scored, uid)
					}
				
				if(res.size == 999) logg.warning("BBBBBBBBBBBBBBBBBBBBB")
				
				Some(ScoredParticles(res))
			} else None
		}
			
		def buildReport[P](gen: Generation[P], config: ABCConfig): Report[P] = {
		    val iteration = gen.currentIteration
		    val tolerance = gen.currentTolerance
		    val weightsTable = gen.prevWeightsTable
		    
			val samples: Seq[P] = Distribution
				.fromProbabilityTable(weightsTable)
				.until(_.size == getters.getNumParticles(config))
				.sample
			
			Report(
				iteration,
				tolerance,
				samples
			)
		}
	}
}