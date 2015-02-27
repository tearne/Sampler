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

package sampler.abc.algorithm

import sampler.Implicits.SamplableMap
import sampler.abc.Model
import sampler.abc.Weighted
import sampler.abc.actor.LoggingAdapterComponent
import sampler.abc.actor.Report
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.Tagged
import sampler.abc.actor.WeighedParticles
import sampler.abc.actor.root.GettersComponent
import sampler.abc.algorithm.component.ToleranceCalculatorComponent
import sampler.abc.config.ABCConfig
import sampler.data.Distribution
import sampler.math.Random
import sampler.math.StatisticsComponent
import scala.collection.immutable.Queue
import sampler.abc.algorithm.component.ParticleMixerComponent

trait AlgorithmComponent {
	val algorithm: Algorithm
}

trait Algorithm {
	def addWeighted[P](incomingWeighted: WeighedParticles[P], gen: Generation[P]): Generation[P]
	def filterAndQueueForWeighing[P](taggedAndScored: ScoredParticles[P], gen: Generation[P]): Generation[P]
	def flushGeneration[P](gen: Generation[P], numParticles: Int, memoryGenerations: Int): Generation[P]
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
	this: ToleranceCalculatorComponent 
		with StatisticsComponent
		with LoggingAdapterComponent
		with ParticleMixerComponent
		with GettersComponent =>
	
	val algorithm: Algorithm
	
	trait AlgorithmImpl extends Algorithm {
		implicit val r = Random
		
		def addWeighted[P](
				incoming: WeighedParticles[P],
				gen: Generation[P]
		): Generation[P] = {
			val weightedParticles = gen.weighted
			
			gen.copy(
					weighted = weightedParticles.add(incoming)
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
					dueWeighing = particlesDueWeighting.add(filtered),
					idsObserved = observedIds ++ filtered.map(_.id)
			)
		}
		
		def flushGeneration[P](gen: Generation[P], numParticles: Int, memoryGenerations: Int): Generation[P] = {
			val dueWeighing = gen.dueWeighing
		    val weightedParticles = gen.weighted
			val currentTolerance = gen.currentTolerance
			val currentIteration = gen.currentIteration
			val model = gen.model
			val queuedIds = gen.idsObserved
			
			assert(numParticles <= weightedParticles.size)
			
			val seqWeighted = weightedParticles.seq.map(_.value) //Strip out tags
			
			def consolidateToWeightsTable[P](model: Model[P], population: Seq[Weighted[P]]): Map[P, Double] = {
				population
				.groupBy(_.params)
				.map{case (k,v) => (k, v.map(_.weight).sum)}
			}
			
			def trimObservedIds(queue: Queue[Long]) = {
				val maxNum = memoryGenerations * numParticles

				val queueSize = queuedIds.size
				
				if(queueSize >= maxNum) {
				  val reducedNum = (memoryGenerations -1) * numParticles
				  val toDrop = queueSize - reducedNum
				  queuedIds.drop(toDrop)
				} else queuedIds
			}
			
			gen.copy(
			    dueWeighing = dueWeighing.empty,
			    weighted = weightedParticles.empty,
				currentTolerance = toleranceCalculator(seqWeighted, currentTolerance),
				currentIteration = currentIteration + 1,
				prevWeightsTable = consolidateToWeightsTable(model, seqWeighted),
				idsObserved = trimObservedIds(queuedIds)
			)
		}
		
		def isEnoughParticles(gen: Generation[_], config: ABCConfig): Boolean =
			gen.weighted.size >= config.job.numParticles
		
		def emptyWeighingBuffer[P](gen: Generation[P]): Generation[P] = 
			gen.copy(dueWeighing = gen.dueWeighing.empty)
			
		//TODO can we simplify tagged and scored parm sets?
		def buildMixPayload[P](gen: Generation[P], abcParameters: ABCConfig): Option[ScoredParticles[P]] = {
			particleMixer.apply(gen, abcParameters)
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