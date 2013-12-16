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

import scala.concurrent.duration._
import scala.concurrent.duration.DurationInt
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.cluster.Cluster
import sampler.abc.ABCModel
import sampler.abc.Scored
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Broadcaster
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.root.state.StateEngineService
import sampler.cluster.abc.actor.TaggedAndScoredParameterSets
import sampler.cluster.abc.actor.worker.AbortableModelRunner
import sampler.cluster.abc.actor.worker.Worker
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.abc.Weighted
import sampler.cluster.abc.parameters.ABCParameters

class RootActor(
		val model0: ABCModel, 
		abcParams: ABCParameters, 
		modelRunner: AbortableModelRunner,
		stateEngine: StateEngineService
) 
		extends Actor 
		with ActorLogging
{
	val model = model0
	val statistics = Statistics
	
	import model._
	import context._
	
	val config = ConfigFactory.load
	val terminateAtTargetGeneration = config.getBoolean("sampler.abc.terminate-at-target-generation")
	val mixingMessageInterval = Duration(config.getMilliseconds("sampler.abc.mixing.rate"), MILLISECONDS)
	log.info("Mixing rate: {}", mixingMessageInterval)
	
	val broadcaster = context.actorOf(Props[Broadcaster], "broadcaster")
	val worker = context.actorOf(Props(new Worker(modelRunner)), "worker")
	
	case class Mix()
	context.system.scheduler.schedule(5.second, mixingMessageInterval, self, Mix)

	implicit val r = Random
	
	def receive = idle
	
	def idle: Receive = {
		case Start(eState) =>
			val client = sender
			val population = eState.state.weightsTable.keysIterator
				.map{value => Weighted(Scored(value, Nil), 1.0)}
				.toSeq
			worker ! Job(population, abcParams)
			
			val sndr = sender
			
			become(busy(stateEngine.setClient(eState, sndr)))
			
			log.info("Inisialised, starting first generation", worker)
		case msg => log.warning(msg.toString)
	}
	
	
	def busy(eState: EncapsulatedState): Receive = {
		case msg: TaggedAndScoredParameterSets[Scored[ParameterSet]] =>
			val sndr = sender

			//TODO can remove the cast somehow?
			val castSeq = msg.seq.asInstanceOf[Seq[Tagged[Scored[eState.model.ParameterSet]]]]
			
			val newEState = stateEngine.add(eState, abcParams, sndr)(castSeq)
			become(busy(newEState))
			checkIfDone(newEState)
			
		case Mix =>
			stateEngine.getMixPayload(eState, abcParams).foreach{message =>
				broadcaster ! message
			}
			case msg => log.warning(msg.toString)
	}
	
	def checkIfDone(eState: EncapsulatedState) {
		val numGenerations = abcParams.job.numGenerations
		
		if(stateEngine.numberAccumulated(eState) >= abcParams.job.numParticles){
			import eState.state._
			log.info("Generation {}/{} complete", currentIteration, numGenerations)
			if(currentIteration == numGenerations){
				val result: Seq[eState.model.ParameterSet] = weightedParameterSets.map(_.value).take(abcParams.job.numParticles) 
				client ! result
				log.info("Number of required generations completed and reported to requestor")
				if(terminateAtTargetGeneration) worker ! Abort
				else startNextRun(eState)
			} else{
				startNextRun(eState)
			}
		}
	}
		
	def startNextRun(eState: EncapsulatedState){
		val newEState = stateEngine.flushGeneration(eState, abcParams.job.numParticles)
		become(busy(newEState))
		
		worker ! Job(newEState.state.weightedParameterSets, abcParams)
	}
}