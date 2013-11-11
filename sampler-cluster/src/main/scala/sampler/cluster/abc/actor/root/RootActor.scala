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
import scala.concurrent.duration.DurationInt
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import sampler.abc.ABCParameters
import sampler.abc.ABCModel
import sampler.cluster.abc.actor.worker.AbortableModelRunner
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import com.typesafe.config.ConfigFactory
import scala.concurrent.duration._
import akka.cluster.Cluster
import akka.actor.Props
import sampler.cluster.abc.actor.worker.Worker
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Broadcaster
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.TaggedAndScoredParameterSets
import sampler.cluster.abc.actor.Job

class RootActor(
		val model0: ABCModel, 
		abcParams: ABCParameters, 
		modelRunner: AbortableModelRunner
) 
		extends Actor 
		with ActorLogging
		with RootStateComponent
		with WeigherComponent
		with ToleranceCalculatorComponent
		with StatisticsComponent
		with ModelComponent
{
	val model = model0
	val weigher = new Weigher{}
	val toleranceCalculator = new ToleranceCalculator{}
	val rootStateInitialiser = new RootStateInitialiser{}
	val statistics = Statistics
	
	import model._
	import context._
	
	val myRemoteAddress = Cluster(system).selfAddress
	
	val config = ConfigFactory.load
	val terminateAtTargetGeneration = config.getBoolean("sampler.abc.terminate-at-target-generation")
	val mixingMessageInterval = Duration(config.getMilliseconds("sampler.abc.mixing.rate"), MILLISECONDS)
	log.info("Mixing rate: {}", mixingMessageInterval)
	
	val broadcaster = context.actorOf(Props[Broadcaster], "broadcaster")
	val worker = context.actorOf(Props(new Worker(modelRunner)), "worker")
	
	context.system.scheduler.schedule(1.second, mixingMessageInterval, self, Mix)
	case class Mix()

	implicit val r = Random
	
	def receive = idle
	
	def idle: Receive = {
		case Start =>
			val client = sender
			val state = rootStateInitialiser(abcParams, client)
			worker ! Job(state.weightedParameterSets, abcParams)
			
			val sndr = sender
			become(busy(state))
			
			log.info("Inisialised, starting first generation", worker)
	}
	
	def busy(state: RootState): Receive = {
		case msg: TaggedAndScoredParameterSets[Scored] =>
			val sndr = sender
			val newState = state
				.add(msg.seq, sndr)
				.pruneObservedIds(abcParams.numParticles)
			become(busy(newState))
			checkIfDone(newState)
			
		case Mix =>
			state.getMixPayload.foreach{message =>
				broadcaster ! message
			}
	}
	
	def checkIfDone(state: RootState) {
		import state._
		if(numberAccumulated >= abcParams.numParticles){
			log.info("Generation {}/{} complete", state.currentIteration, abcParams.numGenerations)
			if(state.currentIteration == abcParams.numGenerations){
				client ! weightedParameterSets.map(_.parameterSet).take(abcParams.numParticles)
				log.info("Number of required generations completed and reported to requestor")
				if(terminateAtTargetGeneration) worker ! Abort
				else startNextRun(state)
			} else{
				startNextRun(state)
			}
		}
	}
		
	def startNextRun(state: RootState){
		val newState = state.flushGeneration(abcParams.numParticles)
		become(busy(newState))
		
		worker ! Job(newState.weightedParameterSets, abcParams)
	}
}