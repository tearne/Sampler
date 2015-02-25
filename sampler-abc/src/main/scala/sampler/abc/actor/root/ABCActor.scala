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

package sampler.abc.actor.root

import scala.concurrent.Future
import scala.concurrent.duration.DurationLong
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.FSM
import akka.actor.actorRef2Scala
import akka.pattern.pipe
import sampler.abc.Model
import sampler.abc.actor.Abort
import sampler.abc.actor.Start
import sampler.abc.algorithm.AlgorithmComponent
import sampler.abc.algorithm.AlgorithmComponentImpl
import sampler.abc.algorithm.Generation
import sampler.abc.algorithm.component.ToleranceCalculatorComponent
import sampler.abc.algorithm.component.WeigherComponent
import sampler.abc.config.ABCConfig
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.abc.actor.Tagged
import sampler.abc.Scored
import sampler.abc.actor.ScoredParticles
import akka.routing.Broadcast
import sampler.abc.actor.WeighedParticles
import sampler.abc.actor.GenerateJob
import sampler.abc.actor.WeighJob
import scala.util.Failure
import scala.util.Success
import sampler.abc.actor.Failed
import sampler.abc.actor.MixPayload
import sampler.abc.actor.ReportCompleted
import sampler.abc.actor.Report
import sampler.math.Statistics
import sampler.abc.actor.LoggingAdapterComponent
import sampler.abc.actor.LoggingAdapterComponentImpl
import sampler.abc.algorithm.component.ParticleMixerComponent

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
    def getFlushingData = FlushingData(client, cancellableMixing)
	def updateGeneration(g: Generation[P]) = copy(generation = g)
}

case class FlushingData(
    client: ActorRef,
    cancellableMixing: Option[Cancellable]
) extends Data {
  def setGeneration[P](g: Generation[P]) = StateData(g, client, cancellableMixing)
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
			log.info("New filtered and queued ({}) => |Q| = {},  from {}", scored.seq.size, updatedGeneration.dueWeighing.size, sender)
			import updatedGeneration._
			sndr ! WeighJob(dueWeighing, prevWeightsTable, currentTolerance)
			log.debug("Allocate weighing of {} particles to {}", updatedGeneration.dueWeighing.size, sndr)
			stay using stateData.copy(generation = updatedGeneration.emptyWeighingBuffer)
			
		case Event(mixP: MixPayload[P], stateData: StateData[P]) =>
			val scored = mixP.tss
			val updatedGeneration = algorithm.filterAndQueueForWeighing(scored, stateData.generation)
			log.info("New filtered and queued ({}) => |Q| = {},  from REMOTE {}", scored.seq.size, updatedGeneration.dueWeighing.size, sender)
			stay using StateData(updatedGeneration, stateData.client, stateData.cancellableMixing)//stateData.copy(generation = updatedGeneration)

		case Event(weighted: WeighedParticles[P], stateData: StateData[P]) =>
			val updatedGen = algorithm.addWeighted(weighted, stateData.generation)

			log.info(s"Generation ${updatedGen.currentIteration}, Particles + ${getters.getNumParticles(weighted)} = ${getters.getAccumulatedGenerationSize(updatedGen)}/${config.job.numParticles}")
			
			if(algorithm.isEnoughParticles(updatedGen, config)){
				router ! Broadcast(Abort)
				
				//Flush the current generation
				implicit val dispatcher = workDispatcher
				Future{
					val flushedGen = algorithm.flushGeneration(updatedGen, config.job.numParticles, config.cluster.particleMemoryGenerations)
					FlushComplete(flushedGen)
				}.pipeTo(self)
				
				goto(Flushing) using stateData.getFlushingData
			} else {
				val updatedState = stateData.updateGeneration(updatedGen)
				allocateWork(sender, updatedState)
			}
			
		case Event(MixNow, p: StateData[P]) => 
			val payload = algorithm.buildMixPayload(p.generation, config)
			payload.foreach{message => broadcaster ! MixPayload(message)}
			
			stay
		
		case Event(rc: ReportCompleted[P], _) => 
		  log.debug(s"Report for generation ${rc.report.generationId} completed.")
		  stay
		
	}
	
	when(Flushing) {
		case Event(_: ScoredParticles[P], _) => 	log.info("Ignore new paylod"); 		stay
		case Event(MixNow, _) => 				log.info("Ignore mix request"); 	stay
		case Event(FlushComplete(flushedGeneration), data: FlushingData) =>
			val numGenerations = config.job.numGenerations
			log.info("Generation {}/{} complete, new tolerance {}", flushedGeneration.currentIteration, numGenerations, flushedGeneration.currentTolerance)
			
			if(flushedGeneration.currentIteration == numGenerations && config.cluster.terminateAtTargetGenerations){
				// Stop work
				router ! Abort  // TODO superfluous?
				report(flushedGeneration)
				log.info("Required generations completed and reported to requestor")
				
				goto(WaitingForShutdown) using data.setGeneration(flushedGeneration)
			} else {
				// Report and start next generation
				router ! Broadcast(GenerateJob(flushedGeneration.prevWeightsTable, config))
				report(flushedGeneration)
				goto(Gathering) using data.setGeneration(flushedGeneration)
			}
		case Event(rc: ReportCompleted[P], _) => 
			log.debug(s"Report for generation ${rc.report.generationId} completed.")
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