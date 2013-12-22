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
import akka.routing.BroadcastRouter
import sampler.cluster.abc.actor.worker.AbortableModelRunnerFactory
import akka.routing.FromConfig
import sampler.cluster.abc.actor.Receiver
import scala.concurrent.Future
import akka.pattern.pipe

class RootActor(
		val model0: ABCModel, 
		abcParams: ABCParameters, 
		modelRunnerFactory: AbortableModelRunnerFactory,
		stateEngine: StateEngineService
) 
		extends Actor 
		with ActorLogging
{
	val model = model0
	val statistics = Statistics
	
	import model._
	import context._
	
	val broadcaster = context.actorOf(Props(classOf[Broadcaster], abcParams), "broadcaster")
	val receiver = context.actorOf(Props[Receiver], "receiver")
	val workerRouter = context.actorOf(FromConfig.props(Props(classOf[Worker],modelRunnerFactory)), "work-router")
	
	
	case class Finished(eState: EncapsulatedState)
	case class NextGeneration(eState: EncapsulatedState)
	
	case class Mix()
	val mixPeriod = abcParams.cluster.mixRateMS.millisecond
	context.system.scheduler.schedule(mixPeriod * 10, mixPeriod, self, Mix)

	
	implicit val r = Random
	
	def receive = idle
	
	def idle: Receive = {
		case Start(eState) =>
			val client = sender
			val population = eState.state.weightsTable.keysIterator
				.map{value => Weighted(Scored(value, Nil), 1.0)}
				.toSeq
			workerRouter ! Job(population, abcParams)
			
			val sndr = sender
			become(gathering(stateEngine.setClient(eState, sndr)))
		case msg => log.error("Unexpected message of type {} when in Idle state",msg.getClass())
	}
	
	
	def gathering(eState: EncapsulatedState): Receive = {
		case data: TaggedAndScoredParameterSets[_] =>
			val sndr = sender
			val castData = data.seq.asInstanceOf[Seq[Tagged[Scored[eState.model.ParameterSet]]]]
			val newEState = stateEngine.add(eState, abcParams, sndr)(castData)
			
			if(stateEngine.numberAccumulated(newEState) < abcParams.job.numParticles)
				become(gathering(newEState))	//Continue gathering particles
			else {
				workerRouter ! Abort
				become(finalisingGeneration)

				implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
				
				Future{
					import newEState.state._
					val numGenerations = abcParams.job.numGenerations
					log.info("Generation {}/{} complete", currentIteration, numGenerations)
					
					if(currentIteration == numGenerations){
						if(abcParams.cluster.terminateAtTargetGenerations) Finished(newEState)
						else NextGeneration(stateEngine.flushGeneration(newEState, abcParams.job.numParticles))
					} 
					else NextGeneration(stateEngine.flushGeneration(newEState, abcParams.job.numParticles))
				}.pipeTo(self)
			}
		case Mix => 
			val payload = stateEngine.getMixPayload(eState, abcParams)
			payload.foreach{message =>
				broadcaster ! message
			}
		case msg => log.error("Unexpected message of type {} when in Gathering state",msg.getClass())
		
	}
	
	def finalisingGeneration(): Receive = {
		case _: TaggedAndScoredParameterSets[_] => //Ignored
		case Mix => //Ignored
			//TODO in future may accumulate
		case Finished(eState) => 
			workerRouter ! Abort

			val result: Seq[eState.model.ParameterSet] = eState.state.weightedParameterSets.map(_.params).take(abcParams.job.numParticles) 
			eState.state.client ! result
			log.info("Number of required generations completed and reported to requestor")
		case NextGeneration(eState) => 
			become(gathering(eState))
			workerRouter ! Job(eState.state.weightedParameterSets, abcParams)
		case msg => log.error("Unexpected message of type {} when in FinalisingGeneration state",msg.getClass())
	}
}