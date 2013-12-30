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
import sampler.abc.ABCModel
import sampler.abc.Scored
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.actor.TaggedAndScoredParameterSets
import sampler.cluster.abc.parameters.ABCParameters
import sampler.cluster.abc.state.EncapsulatedState
import sampler.cluster.abc.state.StateEngineComponent
import sampler.cluster.abc.state.component.ToleranceCalculatorComponent
import sampler.cluster.abc.state.component.WeigherComponent
import sampler.data.Distribution
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.cluster.abc.state.StateEngineComponentImpl
import akka.actor.Cancellable

class RootActorImpl(
		val model: ABCModel,
		val abcParams: ABCParameters
) extends RootActor 
		with ChildrenActorsComponent
		with StateEngineComponentImpl 
		with WeigherComponent
		with ToleranceCalculatorComponent 
		with StatisticsComponent {
	val childrenActors = new ChildrenActors{}
	val weigher = new Weigher{}
	val toleranceCalculator = new ToleranceCalculator{}
	val stateEngine = new StateEngineImpl{}
	val statistics = Statistics
	val random = Random
}

abstract class RootActor
		extends Actor 
		with ActorLogging
{
	this: ChildrenActorsComponent
		with StateEngineComponent =>
	
	import model._
	import context._
	import childrenActors._
	
	case class Finished(eState: EncapsulatedState)
	case class NextGeneration(eState: EncapsulatedState)
	
	case class Mix()
	
	var cancellable: Option[Cancellable] = None
	
//	override def preStart{
//		val mixPeriod = mixRateMSLens.get(abcParams).milliseconds
//		cancellable = Some(
//			context.system.scheduler.schedule(mixPeriod * 10, mixPeriod, self, Mix)
//		)
//	}
//	override def postStop{
//		cancellable.foreach(_.cancel)
//	}

	implicit val r = Random
	
	def receive = idle
	
	def idle: Receive = {
		case Start(eState) =>
			val client = sender
			workerRouter ! Job(eState.state.prevWeightsTable, abcParams)
			
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
					val flushed = stateEngine.flushGeneration(newEState, abcParams.job.numParticles)
					import flushed.state._
					val numGenerations = abcParams.job.numGenerations
					log.info("Generation {}/{} complete, new tolerance {}", currentIteration, numGenerations, currentTolerance)
					
					if(currentIteration == numGenerations && abcParams.cluster.terminateAtTargetGenerations)
						Finished(flushed)
					else 
						NextGeneration(flushed)
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
			//TODO in future may accumulate while running the finalisation future
		case Finished(eState) => 
			workerRouter ! Abort

			val result: Seq[eState.model.ParameterSet] = Distribution
				.fromProbabilityTable(eState.state.prevWeightsTable)
				.until(_.size == abcParams.job.numParticles)
				.sample
			eState.state.client.foreach(_ ! result)
			log.info("Number of required generations completed and reported to requestor")
		case NextGeneration(eState) => 
			become(gathering(eState))
			workerRouter ! Job(eState.state.prevWeightsTable, abcParams)
		case msg => log.error("Unexpected message of type {} when in FinalisingGeneration state",msg.getClass())
	}
}