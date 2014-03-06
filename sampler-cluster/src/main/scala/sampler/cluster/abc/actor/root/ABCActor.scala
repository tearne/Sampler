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
import sampler.cluster.abc.actor.Start
import sampler.cluster.abc.algorithm.AlgorithmComponent
import sampler.cluster.abc.algorithm.AlgorithmComponentImpl
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.algorithm.component.ToleranceCalculatorComponent
import sampler.cluster.abc.algorithm.component.WeigherComponent
import sampler.cluster.abc.config.ABCConfig
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.cluster.abc.actor.Tagged
import sampler.cluster.abc.Scored
import sampler.cluster.abc.actor.ScoredParticles
import akka.routing.Broadcast
import sampler.cluster.abc.actor.WeighedParticles
import sampler.cluster.abc.actor.GenerateJob
import sampler.cluster.abc.actor.WeighJob
import scala.util.Failure
import scala.util.Success
import sampler.cluster.abc.actor.Failed
import sampler.cluster.abc.actor.MixPayload
import sampler.cluster.abc.actor.ReportCompleted
import sampler.cluster.abc.actor.Report
import sampler.math.Statistics
import sampler.cluster.abc.actor.LoggingAdapterComponent
import sampler.cluster.abc.actor.LoggingAdapterComponentImpl
import sampler.cluster.abc.algorithm.component.ParticleMixerComponent

class ABCActorImpl[P](
		val model: Model[P],
		val config: ABCConfig,
		val reportAction: Option[Report[P] => Unit]
) extends ABCActor[P]
		with ChildrenActorsComponent[P]
		with WorkDispatcherComponentImpl
		with AlgorithmComponentImpl 
		with ParticleMixerComponent
		with WeigherComponent
		with ToleranceCalculatorComponent
		with LoggingAdapterComponentImpl
		with StatisticsComponent 
		with GettersComponent {
	val childActors = new ChildActors{}
	val weigher = new Weigher{}
	val toleranceCalculator = new ToleranceCalculator{}
	val particleMixer = new ParticleMixer{}
	val algorithm = new AlgorithmImpl{}
	val statistics = Statistics
	val random = Random
	val getters = new Getters{}
}

sealed trait Status
case object Idle extends Status
case object Gathering extends Status
case object Flushing extends Status
case object WaitingForShutdown extends Status

sealed trait Data{
	val cancellableMixing: Option[Cancellable]
}
case object Uninitialized extends Data{
	val cancellableMixing: Option[Cancellable] = None
}
case class StateData[P](	// TODO change to WorkingData
		generation: Generation[P],
		client: ActorRef, 
		cancellableMixing: Option[Cancellable]
) extends Data{
	def updateGeneration(g: Generation[P]) = copy(generation = g)
}

abstract class ABCActor[P]
		extends FSM[Status, Data] 
		with Actor 
		with ActorLogging
{
	this: ChildrenActorsComponent[P]
		with AlgorithmComponent
		with GettersComponent 
		with WorkDispatcherComponent =>
	
	val config: ABCConfig
	val model: Model[P]
	val reportAction: Option[Report[P] => Unit]
	
	import childActors._
	type G = Generation[P]
	
	case class FlushComplete(generation: G)
	case class AddComplete(generation: G)
	case object MixNow
	
	startWith(Idle, Uninitialized)

	onTermination{
		case se: StopEvent => se.stateData.cancellableMixing.foreach(_.cancel)
	}

	onTransition{
		case _ -> Idle => stateData.cancellableMixing.foreach(_.cancel)
	}
	
	whenUnhandled{
		case Event(msg, _) =>
			log.error(s"Unhandled message ${msg.getClass} in state $stateName")
			stay
	}
		
	//TODO this is were you should be looking
	when(Idle) {
		case Event(s:Start[P], Uninitialized) =>
			val client = sender
			import s._
			
			router ! Broadcast(GenerateJob(generationZero.prevWeightsTable, config))
			
			// TODO this code block untested
			val mixMS = getters.getMixRateMS(config)
			assert(mixMS > 0l, "Mixing rate must be strictly positive")
			val cancellableMixing = Some(
				context.system.scheduler.schedule(
						mixMS.milliseconds,
						mixMS.milliseconds, 
						self, 
						MixNow)(
						context.dispatcher)
			)
			
			goto(Gathering) using StateData(generationZero, client, cancellableMixing)
	}
	
	when(Gathering) {
		case Event(Failed, stateData: StateData[P]) => 
			log.error("FAILURE ENCOUNTERED IN WORKER")
			allocateWork(sender, stateData)
			
		case Event(scored: ScoredParticles[P], stateData: StateData[P]) =>
			val sndr = sender
			val updatedGeneration = algorithm.filterAndQueueForWeighing(scored, stateData.generation)
			log.info("filterAndQueue({}) => |W| = {},  from {}", scored.seq.size, updatedGeneration.dueWeighing.size, sender)
			import updatedGeneration._
			sndr ! WeighJob(dueWeighing, prevWeightsTable, currentTolerance)
			log.debug("Allocate weighing of {} particles to {}", updatedGeneration.dueWeighing.size, sndr)
			stay using stateData.copy(generation = updatedGeneration.emptyWeighingBuffer)
			
		case Event(mixP: MixPayload[P], stateData: StateData[P]) =>
			val scored = mixP.tss
			val updatedGeneration = algorithm.filterAndQueueForWeighing(scored, stateData.generation)
			log.info("filterAndQueue({}) => |W| = {},  from REMOTE {}", scored.seq.size, updatedGeneration.dueWeighing.size, sender)
			stay using StateData(updatedGeneration, stateData.client, stateData.cancellableMixing)//stateData.copy(generation = updatedGeneration)

		case Event(weighted: WeighedParticles[P], stateData: StateData[P]) =>
			val updatedGen = algorithm.addWeighted(weighted, stateData.generation)
//			log.info(s"Currently G${updatedGen.currentIteration}, Particles + ${weighted.seq.size} = ${updatedGen.weighted.size}/${config.job.numParticles}")
			
			if(algorithm.isEnoughParticles(updatedGen, config)){
				router ! Broadcast(Abort)
				
				//Flush the current generation
				// TODO test the end result of this code 
				implicit val dispatcher = workDispatcher
				Future{
					val flushedGen = algorithm.flushGeneration(updatedGen, config.job.numParticles, config.cluster.particleMemoryGenerations)
					FlushComplete(flushedGen)
				}.pipeTo(self)
				
				goto(Flushing) using stateData.updateGeneration(updatedGen)
			} else {
				val updatedState = stateData.updateGeneration(updatedGen)
				allocateWork(sender, updatedState)
			}
			
		case Event(MixNow, p: StateData[P]) => 
			val payload = algorithm.buildMixPayload(p.generation, config)
			payload.foreach{message => broadcaster ! MixPayload(message)}
			
			stay
		
		case Event(_: ReportCompleted[P], _) => stay
	}
	
	when(Flushing) {
		case Event(_: ScoredParticles[P], _) => 	log.info("Ignore new paylod"); 		stay
		case Event(MixNow, _) => 				log.info("Ignore mix request"); 	stay
		case Event(FlushComplete(flushedGeneration), data: StateData[P]) =>
			import flushedGeneration._
			val numGenerations = config.job.numGenerations
			println("KNELNGESNESGONEOINGE")
			println("HERE!! " + currentIteration)
			println("HERE!! " + numGenerations)
			println("HERE!! " + currentTolerance)
			log.info("Generation {}/{} complete, new tolerance {}", currentIteration, numGenerations, currentTolerance)
			
			if(currentIteration == numGenerations && config.cluster.terminateAtTargetGenerations){
				// Stop work
				router ! Abort  // TODO superfluous?
				report(flushedGeneration)
				log.info("Required generations completed and reported to requestor")
				
				goto(WaitingForShutdown) using data
			} else {
				// Report and start next generation
				router ! Broadcast(GenerateJob(flushedGeneration.prevWeightsTable, config))
				report(flushedGeneration)
				goto(Gathering) using data.updateGeneration(flushedGeneration)
			}
		case Event(rc: ReportCompleted[P], data: StateData[P]) => 
			log.warning(s"Current generation ${data.generation.currentIteration} but report from a generation ${rc.report.generationId} only just finished.  System is running slowly.")
			stay
	}
	
	when(WaitingForShutdown) {
		case Event(rc: ReportCompleted[P], state: StateData[P]) => 
			log.info("Final report completed.  Informing client")
			state.client ! rc.report
			stay
	}
	
	def allocateWork(worker: ActorRef, stateData: StateData[P]) = {
		val generation = stateData.generation
		import generation._
		
		if(dueWeighing.size > 0) {
			// Tell worker to weigh particles
			worker ! WeighJob(dueWeighing, prevWeightsTable, currentTolerance)
			stay using stateData.updateGeneration(algorithm.emptyWeighingBuffer(generation))	//Weigh existing particles
		} else {
			// Tell worker to make more particles
			worker ! GenerateJob(prevWeightsTable, config)	
			stay using stateData.updateGeneration(generation)
		}
	}
	
	def report(generation: Generation[P]){
		reportingActor ! algorithm.buildReport(generation, config)
	}
}