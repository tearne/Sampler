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

import scala.concurrent.Future
import scala.concurrent.duration.DurationLong
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.actorRef2Scala
import akka.pattern.pipe
import sampler.cluster.abc.Scored
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.state.StateEngineComponent
import sampler.cluster.abc.state.component.ToleranceCalculatorComponent
import sampler.cluster.abc.state.component.WeigherComponent
import sampler.data.Distribution
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.cluster.abc.state.StateEngineComponentImpl
import akka.actor.Cancellable
import sampler.cluster.abc.Model
import sampler.cluster.abc.state.State
import akka.actor.ActorRef
import sampler.cluster.abc.actor.Lenses

class RootActorImpl[P](
		val model: Model[P],
		val config: ABCConfig
) extends RootActor[P]
		with ModelAndConfig[P]
		with ChildrenActorsComponent[P]
		with StateEngineComponentImpl 
		with WeigherComponent
		with ToleranceCalculatorComponent 
		with StatisticsComponent 
		with Lenses {
	val childrenActors = new ChildrenActors{}
	val weigher = new Weigher{}
	val toleranceCalculator = new ToleranceCalculator{}
	val stateEngine = new StateEngineImpl{}
	val statistics = Statistics
	val random = Random
}

abstract class RootActor[P]
		extends Actor 
		with ActorLogging
{
	this: ChildrenActorsComponent[P]
		with ModelAndConfig[P]
		with StateEngineComponent
		with Lenses =>
	
	import model._
	import context._
	import childrenActors._
	
	type S = State[P]
	
	case class Finished(state: S)
	case class NextGeneration(state: S)
	
	case class Mix()
	
	var cancellable: Option[Cancellable] = None
	
	override def preStart{
		val mixPeriod = mixRateMS.get(config).milliseconds
		cancellable = Some(
			context.system.scheduler.schedule(mixPeriod * 10, mixPeriod, self, Mix)
		)
	}
	override def postStop{
		cancellable.foreach(_.cancel)
	}

	implicit val r = Random
	
	def receive = idle
	
	def idle: Receive = {
		case s:Start[P] =>
			val client = sender
			workerRouter ! Job(s.init.prevWeightsTable, config)
			become(gathering(s.init, client))
		case msg => log.error("Unexpected message of type {} when in Idle state",msg.getClass())
	}
	
	
	def gathering(state: S, requestor: ActorRef): Receive = {
		case data: TaggedScoreSeq[P] =>
			val sndr = sender
			val castData = data.seq.asInstanceOf[Seq[Tagged[Scored[P]]]]
			val newState: S = stateEngine.add(state, config, castData, sndr)
			
			if(stateEngine.numberAccumulated(newState) < config.job.numParticles)
				become(gathering(newState, requestor))	//Continue gathering particles
			else {
				workerRouter ! Abort
				become(finalisingGeneration(requestor))

				implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
				
				Future{
					val flushed = stateEngine.flushGeneration(newState, config.job.numParticles)
					import flushed._
					val numGenerations = config.job.numGenerations
					log.info("Generation {}/{} complete, new tolerance {}", currentIteration, numGenerations, currentTolerance)
					
					if(currentIteration == numGenerations && config.cluster.terminateAtTargetGenerations)
						Finished(flushed)
					else 
						NextGeneration(flushed)
				}.pipeTo(self)
			}
		case Mix => 
			val payload = stateEngine.buildMixPayload(state, config)
			payload.foreach{message =>
				broadcaster ! message
			}
		case msg => log.error("Unexpected message of type {} when in Gathering state",msg.getClass())
		
	}
	
	def finalisingGeneration(client: ActorRef): Receive = {
		case _: TaggedScoreSeq[P] => 	//Ignored
		case Mix => 					//Ignored
		case Finished(state) => 
			workerRouter ! Abort
			report(client, state, true)
			log.info("Number of required generations completed and reported to requestor")
		case NextGeneration(state) => 
			report(client, state, false)
			become(gathering(state, client))
			workerRouter ! Job(state.prevWeightsTable, config)
		case msg => log.error("Unexpected message of type {} when in FinalisingGeneration state",msg.getClass())
	}
	
	def report(client: ActorRef, state: State[P], finalReport: Boolean){
		client ! stateEngine.buildReport(state, config, finalReport)
	}
}