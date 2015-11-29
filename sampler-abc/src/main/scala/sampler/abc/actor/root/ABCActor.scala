/*
 * Copyright (c) 2012-15 Crown Copyright 
 *                       Animal and Plant Health Agency
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
import sampler.abc.config.ABCConfig
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.abc.actor.Tagged
import sampler.abc.Scored
import sampler.abc.actor.ScoredParticles
import akka.routing.Broadcast
import sampler.abc.actor.WeighedParticles
import sampler.abc.actor.WeighJob
import scala.util.Failure
import scala.util.Success
import sampler.abc.actor.Failed
import sampler.abc.actor.MixPayload
import sampler.abc.actor.ReportCompleted
import sampler.abc.actor.Report
import sampler.math.Statistics
import sampler.abc.core.LoggingAdapterComponent
import sampler.abc.core.LoggingAdapterComponentImpl
import sampler.abc.actor.GenerateParticlesFrom
import sampler.abc.actor.algorithm.Algorithm
import sampler.abc.actor.algorithm.EvolvingGeneration
import sampler.abc.core.Generation
import sampler.abc.actor.algorithm.Getters
import sampler.abc.core.WeightsHelper
import sampler.abc.core.ToleranceCalculator
import sampler.abc.actor.algorithm.ParticleMixer
import sampler.abc.actor.algorithm.ObservedIdsTrimmer
import sampler.abc.actor.algorithm.GenerationFlusher
import sampler.abc.actor.algorithm.ObservedIdsTrimmer
import sampler.data.DistributionBuilder
import sampler.abc.core.Reporter
import sampler.abc.core.Reporter
import sampler.abc.core.Reporter
import sampler.abc.actor.FlushComplete

class ABCActorImpl[P](
		val model: Model[P],
		val config: ABCConfig,
		val reportAction: Option[Report[P] => Unit]
	) extends ABCActor[P]
		with ChildrenActorsComponentImpl[P]
		with WorkDispatcherComponentImpl
		with LoggingAdapterComponentImpl
		with AlgorithmCoponentImpl
		with StatisticsComponent {

	val distributionBuilder = DistributionBuilder
	
	val getters = new Getters()
	val reporter = new Reporter(
			DistributionBuilder,
			Random,
			config)
	val statistics = Statistics
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
		generation: EvolvingGeneration[P],
		client: ActorRef, 
		cancellableMixing: Option[Cancellable]
) extends Data{
  def getFlushingData = FlushingData(client, cancellableMixing)
	def updateGeneration(g: EvolvingGeneration[P]) = copy(generation = g)
}

case class FlushingData(
    client: ActorRef,
    cancellableMixing: Option[Cancellable]
) extends Data {
  def setGeneration[P](g: EvolvingGeneration[P]) = StateData(g, client, cancellableMixing)
}

trait ABCActor[P]
		extends FSM[Status, Data] 
		with Actor 
		with ActorLogging
		with AlgorithmComponent
{
	this: ChildrenActorsComponent[P] with WorkDispatcherComponent =>
	
	val config: ABCConfig
	val model: Model[P]
	val reporter: Reporter	//TODO better name to help distinguish between report action and reporting actor
	val reportAction: Option[Report[P] => Unit]
	val getters: Getters
	
	case class AddComplete(generation: EvolvingGeneration[P])
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
			
			val evolvingGen = EvolvingGeneration.init(s.generationZero)
			childActors.router ! Broadcast(GenerateParticlesFrom(//TODO fix duplication with 'allocateWork' below
					evolvingGen.previousGen, 
					config
			))
			
			// TODO this code block untested
			val mixMS = getters.getMixRateMS(config)
			val cancellableMixing = 
				if(mixMS > 0)
					Some(
						context.system.scheduler.schedule(
								mixMS.milliseconds,
								mixMS.milliseconds, 
								self, 
								MixNow)(
								context.dispatcher)
					)
				else None
			goto(Gathering) using StateData(evolvingGen, client, cancellableMixing)
	}
	
	when(Gathering) {
		case Event(Failed, stateData: StateData[P]) => 
			log.error("FAILURE ENCOUNTERED IN WORKER")
			allocateWork(sender, stateData)
			
		case Event(scored: ScoredParticles[P], stateData: StateData[P]) =>
			val sndr = sender
			val updatedGeneration = algorithm.filterAndQueueUnweighedParticles(scored, stateData.generation)
			log.info("New filtered and queued ({}) => |Q| = {},  from {}", scored.seq.size, updatedGeneration.dueWeighing.size, sender)
			sndr ! WeighJob.buildFrom(updatedGeneration)
			log.debug("Allocate weighing of {} particles to {}", updatedGeneration.dueWeighing.size, sndr)
			stay using stateData.copy(generation = updatedGeneration.emptyWeighingBuffer)
			
		case Event(mixP: MixPayload[P], stateData: StateData[P]) =>
			val scored = mixP.scoredParticles
			val updatedGeneration = algorithm.filterAndQueueUnweighedParticles(scored, stateData.generation)
			log.info("New filtered and queued ({}) => |Q| = {},  from REMOTE {}", scored.seq.size, updatedGeneration.dueWeighing.size, sender)
			stay using StateData(updatedGeneration, stateData.client, stateData.cancellableMixing)//stateData.copy(generation = updatedGeneration)

		case Event(weighted: WeighedParticles[P], stateData: StateData[P]) =>
			val updatedGen = algorithm.addWeightedParticles(weighted, stateData.generation)

			log.info(
					s"Working on gen ${updatedGen.buildingGeneration}, "+
					s"Particles + ${getters.getNumParticles(weighted)} = "+
					s"${getters.getNumEvolvedParticles(updatedGen)}/${config.job.numParticles}")
			
			if(algorithm.isEnoughParticles(updatedGen, config)){
				childActors.router ! Broadcast(Abort)
				childActors.flusher ! updatedGen
				
				goto(Flushing) using stateData.getFlushingData
			} else {
				val updatedState = stateData.updateGeneration(updatedGen)
				allocateWork(sender, updatedState)
			}
			
		case Event(MixNow, p: StateData[P]) => 
			val payload = algorithm.buildMixPayload(p.generation, config)
			payload.foreach{message => 
				childActors.broadcaster ! MixPayload(message)}
			
			stay
		
		case Event(rc: ReportCompleted[P], _) => 
		  log.debug(s"Report for generation ${rc.report.generationId} completed.")
		  stay
	}
	
	when(Flushing) {
		case Event(_: ScoredParticles[P], _) => 	log.info("Ignore new paylod"); 		stay
		case Event(MixNow, _) => 				log.info("Ignore mix request"); 	stay
		case Event(fc: FlushComplete[P], data: FlushingData) =>
			val numReqGenerations = config.job.numGenerations
			val flushedEGen = fc.eGeneration
			val generationCompleted = flushedEGen.previousGen.iteration
			
			log.info(  //TODO new logging trait
					"Generation {}/{} complete, next tolerance {}", 
					generationCompleted, 
					numReqGenerations, 
					flushedEGen.currentTolerance
			)
			
			if(generationCompleted == numReqGenerations && config.cluster.terminateAtTargetGenerations){
				// Stop work
				childActors.router ! Abort  // TODO superfluous?
				reportCompletedGeneration(flushedEGen.previousGen)
				log.info("Required generations completed and reported to requestor")
				
				goto(WaitingForShutdown) using data.setGeneration(flushedEGen)
			} else {
				// Start next generation
				//TODO fix duplication with 'allocateWork' below
				childActors.router ! Broadcast(GenerateParticlesFrom( //TODO check test coverage
						flushedEGen.previousGen,
						config
				))
				reportCompletedGeneration(flushedEGen.previousGen)
				goto(Gathering) using data.setGeneration(flushedEGen)
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
			worker ! WeighJob.buildFrom(generation)
			stay using stateData.updateGeneration(algorithm.emptyWeighingBuffer(generation))	//Weigh existing particles
		} else {
			// Tell worker to make more particles
			worker ! GenerateParticlesFrom(generation.previousGen, config)
			stay using stateData.updateGeneration(generation)
		}
	}
	
	def reportCompletedGeneration(generation: Generation[P]){
		childActors.reporter ! reporter.build(generation)
	}
}