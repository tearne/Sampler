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
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.FSM
import akka.actor.actorRef2Scala
import akka.pattern.pipe
import sampler.cluster.abc.Model
import sampler.cluster.abc.actor.Abort
import sampler.cluster.abc.actor.Job
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.actor.TaggedScoreSeq
import sampler.cluster.abc.algorithm.AlgorithmComponent
import sampler.cluster.abc.algorithm.AlgorithmComponentImpl
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.algorithm.component.ToleranceComponent
import sampler.cluster.abc.algorithm.component.WeigherComponent
import sampler.cluster.abc.config.ABCConfig
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.Scored

class RootActorImpl[P](
		val model: Model[P],
		val config: ABCConfig
) extends RootActor[P]
		with ChildrenActorsComponent[P]
		with AlgorithmComponentImpl 
		with WeigherComponent
		with ToleranceComponent 
		with StatisticsComponent 
		with GettersComponent {
	val childActors = new ChildActors{}
	val weigher = new Weigher{}
	val toleranceCalculator = new ToleranceCalculator{}
	val algorithm = new AlgorithmImpl{}
	val statistics = Statistics
	val random = Random
}

sealed trait Status
case object Idle extends Status
case object Gathering extends Status
case object Adding extends Status
case object Backlogged extends Status
case object Flushing extends Status

sealed trait Data{
	val mixCallback: Option[Cancellable] = None
}
case object Uninitialized extends Data
case class GatheringData[P](
		generation: Generation[P], 
		client: ActorRef, 
		cancellableMixing: Option[Cancellable]
		) extends Data{
	def toWaitingData = WaitingData(client, cancellableMixing, Seq.empty[Tagged[Scored[P]]])
}
case class WaitingData[P](
		client: ActorRef, 
		cancellableMixing: Option[Cancellable],
		queued: Seq[Tagged[Scored[P]]]
		) extends Data {
	def toGatheringData[P](generation: Generation[P]) = 
		GatheringData(generation, client, cancellableMixing)
	def emptyQueue = copy(queued = Seq.empty[Tagged[Scored[P]]])
}

abstract class RootActor[P]
		extends FSM[Status, Data] 
		with Actor 
		with ActorLogging
{
	this: ChildrenActorsComponent[P]
		with AlgorithmComponent
		with GettersComponent =>
	
	val config: ABCConfig
	val model: Model[P]
	
	implicit val executionContext = context.system.dispatchers.lookup("sampler.work-dispatcher")
			
	import childActors._
	type G = Generation[P]
	
	case class FlushComplete(generation: G)
	case class AddComplete(generation: G)
	case object MixNow
	
	startWith(Idle, Uninitialized)

	onTermination{
		case se: StopEvent => se.stateData.mixCallback.foreach(_.cancel)
	}

	onTransition{
		case _ -> Idle => stateData.mixCallback.foreach(_.cancel)
	}
		
	when(Idle) {
		case Event(s:Start[P], Uninitialized) =>
			val client = sender
			import s._
			
			workerRouter ! Job(generationZero.prevWeightsTable, config)
			
			val mixMS = getters.getMixRateMS(config)
			assert(mixMS > 0l, "Mixing rate must be strictly positive")
			val cancellableMixing = Some(
				context.system.scheduler.schedule(
						mixMS.milliseconds * 10,
						mixMS.milliseconds, 
						self, 
						MixNow)(
						context.dispatcher)
			)
			
			goto(Gathering) using GatheringData(generationZero, client, cancellableMixing)
	}
	
	when(Gathering) {
		case Event(newData: TaggedScoreSeq[P], gData: GatheringData[P]) =>
			val sndr = sender
			
			Future{
				val newGen: G = algorithm.add(gData.generation, newData.seq, config)
				AddComplete(newGen)
			}.pipeTo(self)
			
			goto(Adding) using gData.toWaitingData
			
		case Event(MixNow, p: GatheringData[P]) => 
			val payload = algorithm.buildMixPayload(p.generation, config)
			payload.foreach{message => broadcaster ! message}
			
			stay
	}
	
	when(Adding){
		case Event(tss: TaggedScoreSeq[P], data: WaitingData[P]) => 	
			val newData = data.copy(queued = data.queued ++ tss.seq)
			log.info("Queue paylod, size = {}", newData.queued.size)
			stay using newData
		case Event(MixNow, _) => 
			log.info("Ignore mix request"); 	
			stay
		case Event(AddComplete(generation), waitingData: WaitingData[P]) =>
			if(algorithm.numberAccumulated(generation) < config.job.numParticles){
				if(waitingData.queued.size > 3 * config.algorithm.particleChunkSize){
					workerRouter ! Abort
					val queue = waitingData.queued
					log.warning(s"OVERLOAD, work aborted. Q size ${queue.size}")
					Future{
						val newGen: G = algorithm.add(generation, queue, config)
						AddComplete(newGen)
					}.pipeTo(self)
					goto(Backlogged) using waitingData.emptyQueue
				} else {
					self ! TaggedScoreSeq(waitingData.queued)	//Send the queued stuff back to the mailbox
					goto(Gathering) using waitingData.toGatheringData(generation) 	
				}
				
			} else {
				//Flush the current generation
				workerRouter ! Abort
				Future{
					val flushedGen = algorithm.flushGeneration(generation, config.job.numParticles)
					FlushComplete(flushedGen)
				}.pipeTo(self)
				
				goto(Flushing) using waitingData
			}
	}
	
	when(Backlogged) {
		case Event(_: TaggedScoreSeq[P], _) => 	log.info("Ignore new paylod from {}", sender); stay
		case Event(MixNow, _) => 				log.info("Ignore mix request"); 	stay
		
		case Event(AddComplete(generation), waitingData: WaitingData[P]) =>
			// Backlog cleared, start work and gathering again
			if(algorithm.numberAccumulated(generation) < config.job.numParticles){
				workerRouter ! Job(generation.prevWeightsTable, config)
				goto(Gathering) using waitingData.toGatheringData(generation) 	
			} else {
				//Flush the current generation
				workerRouter ! Abort
				Future{
					val flushedGen = algorithm.flushGeneration(generation, config.job.numParticles)
					FlushComplete(flushedGen)
				}.pipeTo(self)
				
				goto(Flushing) using waitingData.emptyQueue
			}
	}
	
	when(Flushing) {
		case Event(_: TaggedScoreSeq[P], _) => 	log.info("Ignore new paylod"); 		stay
		case Event(MixNow, _) => 				log.info("Ignore mix request"); 	stay
		case Event(FlushComplete(generation), data: WaitingData[P]) =>
			import generation._
			val numGenerations = config.job.numGenerations
			log.info("Generation {}/{} complete, new tolerance {}", currentIteration, numGenerations, currentTolerance)
			
			if(currentIteration == numGenerations && config.cluster.terminateAtTargetGenerations){
				// Stop work
				workerRouter ! Abort
				report(data.client, generation, true)
				log.info("Number of required generations completed and reported to requestor")
				
				goto(Idle) using Uninitialized
			} else {
				// Report and start next generation
				log.debug("New job sending...")
				workerRouter ! Job(generation.prevWeightsTable, config)
				report(data.client, generation, false)
				goto(Gathering) using data.toGatheringData(generation)
			}
	}
	
	def report(client: ActorRef, generation: Generation[P], finalReport: Boolean){
		client ! algorithm.buildReport(generation, config, finalReport)
	}
}