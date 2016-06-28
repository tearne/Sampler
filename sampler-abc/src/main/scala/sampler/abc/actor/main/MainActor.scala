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

package sampler.abc.actor.main

import scala.concurrent.duration.DurationLong
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.FSM
import akka.actor.actorRef2Scala
import akka.routing.Broadcast
import sampler.abc.Generation
import sampler.abc.Model
import sampler.abc.actor.main.component.ChildActorsComponent
import sampler.abc.actor.main.component.ChildActorsComponentImpl
import sampler.abc.actor.main.component.HelperComponent
import sampler.abc.actor.main.component.HelperCoponentImpl
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.actor.sub.Abort
import sampler.abc.actor.sub.FlushComplete
import sampler.abc.actor.sub.GenerateParticlesFrom
import sampler.abc.actor.sub.WeighJob
import sampler.math.Random
import sampler.math.Statistics
import sampler.math.StatisticsComponent
import sampler.abc.actor.sub.StatusReport
import sampler.abc.actor.sub.NewScored
import sampler.abc.Population
import sampler.abc.actor.sub.FinishGen
import sampler.abc.actor.sub.NewWeighed
import sampler.abc.ABCConfig

/*
 * States/Data
 */
sealed trait Status
case object Idle extends Status
case object Gathering extends Status
case object Flushing extends Status
case object WaitingForShutdown extends Status

sealed trait Data {
	val cancellableMixing: Option[Cancellable]
}
case object Uninitialized extends Data {
	val cancellableMixing: Option[Cancellable] = None
}
case class StateData[P]( // TODO change to WorkingData
		generation: EvolvingGeneration[P],
		client: ActorRef,
		cancellableMixing: Option[Cancellable]) extends Data {
	def getFlushingData = FlushingData(client, cancellableMixing)
	def updateGeneration(g: EvolvingGeneration[P]) = copy(generation = g)
}

case class FlushingData(
		client: ActorRef,
		cancellableMixing: Option[Cancellable]) extends Data {
	def setGeneration[P](g: EvolvingGeneration[P]) = StateData(g, client, cancellableMixing)
}

class MainActorImpl[P](
	val model: Model[P],
	val config: ABCConfig,
	val reportHandler: Option[Population[P] => Unit]) extends MainActor[P]
		with ChildActorsComponentImpl[P]
		with HelperCoponentImpl {

	val getters = new Getters()
}

trait MainActor[P]
		extends FSM[Status, Data]
		with Actor {
	this: ChildActorsComponent[P] with HelperComponent =>

	val config: ABCConfig
	val model: Model[P]
	val getters: Getters

	case class AddComplete(generation: EvolvingGeneration[P])

	startWith(Idle, Uninitialized)

	onTermination {
		case se: StopEvent => se.stateData.cancellableMixing.foreach(_.cancel)
	}

	onTransition {
		case _ -> Idle => stateData.cancellableMixing.foreach(_.cancel)
	}

	whenUnhandled {
		case Event(msg, _) =>
			log.error(s"Unhandled message ${msg.getClass} in state $stateName")
			stay
	}

	when(Idle) {
		case Event(s: Start[P], Uninitialized) =>
			val client = sender
			import s._

			val evolvingGen = helper.initialiseEvolvingGeneration(s.initGeneration, config)
		  childActors.reporter ! StatusReport(
				FinishGen(evolvingGen.previousGen.iteration, evolvingGen.currentTolerance),
				evolvingGen,
				config
			)
			
			childActors.router ! Broadcast(GenerateParticlesFrom( //TODO fix duplication with 'allocateWork' below
				evolvingGen.previousGen,
				config))
				
			// TODO this block currently untested
			val mixMS = config.mixRateMS
			val cancellableMixing =
				if (mixMS > 0)
					Some(
						context.system.scheduler.schedule(
							mixMS.milliseconds,
							mixMS.milliseconds,
							self,
							MixNow)(
								context.dispatcher))
				else None
			goto(Gathering) using StateData(evolvingGen, client, cancellableMixing)
	}

	when(Gathering) {
		case Event(Failed, stateData: StateData[P]) =>
			log.error("FAILURE ENCOUNTERED IN WORKER")
			allocateWork(sender, stateData)

		case Event(scored: ScoredParticles[P], stateData: StateData[P]) =>
			val sndr = sender
			val updatedGeneration = helper.filterAndQueueUnweighedParticles(scored, stateData.generation)
			childActors.reporter ! StatusReport( //TODO untested
					NewScored(scored.seq.size, sender, false),
					updatedGeneration,
					config
			)
			sndr ! WeighJob.buildFrom(updatedGeneration)
			log.debug("Allocate weighing of {} particles to {}", updatedGeneration.dueWeighing.size, sndr)
			stay using stateData.copy(generation = updatedGeneration.emptyWeighingBuffer)

		case Event(weighted: WeighedParticles[P], stateData: StateData[P]) =>
			val updatedGen = helper.addWeightedParticles(weighted, stateData.generation)

			childActors.reporter ! StatusReport(
				NewWeighed(getters.getNumParticles(weighted)),
				updatedGen,
				config
			)
			if (helper.isEnoughParticles(updatedGen, config)) {
				childActors.router ! Broadcast(Abort)
				childActors.flusher ! updatedGen

				goto(Flushing) using stateData.getFlushingData
			} else {
				val updatedState = stateData.updateGeneration(updatedGen)
				allocateWork(sender, updatedState)
			}

		case Event(MixNow, p: StateData[P]) =>
			val payload = helper.buildMixPayload(p.generation, config)
			payload.foreach { message =>
				childActors.broadcaster ! MixPayload(message)
			}

			stay

		case Event(mixP: MixPayload[P], stateData: StateData[P]) =>
			val scored = mixP.scoredParticles
			val updatedGeneration = helper.filterAndQueueUnweighedParticles(scored, stateData.generation)
			childActors.reporter ! StatusReport(//TODO untested
					NewScored(scored.seq.size, sender, true),
					updatedGeneration,
					config
			)
			stay using StateData(updatedGeneration, stateData.client, stateData.cancellableMixing)

		case Event(ReportCompleted, _) =>
			stay
	}

	when(Flushing) {
		case Event(_: ScoredParticles[P], _) =>
			stay
		case Event(MixNow, _) =>
			stay
		case Event(fc: FlushComplete[P], data: FlushingData) =>
			val flushedEGen = fc.eGeneration
			val generationCompleted = flushedEGen.previousGen.iteration

			childActors.reporter ! StatusReport(
				FinishGen(generationCompleted, flushedEGen.currentTolerance),
				flushedEGen,
				config
			)

			if (generationCompleted >= config.numGenerations && config.terminateAtTargetGen) {
				// Stop work
				childActors.router ! Abort // TODO superfluous?
				childActors.reporter ! flushedEGen.previousGen //TODO check test coverage
				goto(WaitingForShutdown) using data.setGeneration(flushedEGen)
			} else {
				// Start next generation
				//TODO fix duplication with 'allocateWork' below
				childActors.router ! Broadcast(GenerateParticlesFrom( //TODO check test coverage
					flushedEGen.previousGen,
					config))
				childActors.reporter ! flushedEGen.previousGen //TODO check test coverage
				goto(Gathering) using data.setGeneration(flushedEGen)
			}
		case Event(ReportCompleted, _) =>
			stay
	}

	when(WaitingForShutdown) {
		case Event(ReportCompleted, state: StateData[P]) =>
			state.client ! state.generation.previousGen
			stay
	}

	def allocateWork(worker: ActorRef, stateData: StateData[P]) = {
		val generation = stateData.generation
		import generation._

		if (dueWeighing.size > 0) {
			// Tell worker to weigh particles
			worker ! WeighJob.buildFrom(generation)
			stay using stateData.updateGeneration(helper.emptyWeighingBuffer(generation)) //Weigh existing particles
		} else {
			// Tell worker to make more particles
			worker ! GenerateParticlesFrom(generation.previousGen, config)
			stay using stateData.updateGeneration(generation)
		}
	}
}
