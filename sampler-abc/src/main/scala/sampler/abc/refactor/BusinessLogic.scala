package sampler.abc.refactor

import akka.actor.ActorRef
import akka.routing.Broadcast
import sampler.abc.ABCConfig
import sampler.abc.actor.main.component.Helper
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.actor.main.{MixPayload, ScoredParticles, WeighedParticles}
import sampler.abc.actor.sub._
import sampler.abc.refactor.Messages.Start
import sampler.io.Logging

class BusinessLogic[P](
    helper: Helper,
    config: ABCConfig,
    childActors: LocalActors[P],
    getters: Getters) extends Logging {
  def buildInitialworkingData(startMsg: Start[P], client: ActorRef): StateData[P] ={
    StateData(
      config.numParticles,
      client,
      helper.initialiseEvolvingGeneration(startMsg.initGen, config)
    )
  }

  def workerFailed(state: StateData[P], worker: ActorRef) {
    warn("Failure in worker, resending job.")
    allocateWorkerTask(state, worker)
  }

  def addLocallyScoredParticles(
      state: StateData[P],
      scored: ScoredParticles[P],
      worker: ActorRef): StateData[P] = {

    val newState = {
      val updatedEGen = helper.filterAndQueueUnweighedParticles(
        scored,
        state.evolvingGeneration
      )
      state.updateEvolvingGeneration(updatedEGen)
    }

    // Report on the new workingData
    childActors.reporter ! StatusReport( //TODO untested
      NewScored(scored.seq.size, worker, false),
      newState.evolvingGeneration,
      config
    )

    allocateWorkerTask(newState, worker)

    newState
  }

  def addScoredFromMixing(mixP: MixPayload[P], state: StateData[P], sender: ActorRef): StateData[P] = {
    val newState = {
      val newEGen = helper.filterAndQueueUnweighedParticles(
        mixP.scoredParticles,
        state.evolvingGeneration)
      state.updateEvolvingGeneration(newEGen)
    }

    childActors.reporter ! StatusReport(//TODO untested
      NewScored(mixP.scoredParticles.seq.size, sender, true),
      newState.evolvingGeneration,
      config
    )

    newState
  }

  def addNewWeighted(state: StateData[P], weighed: WeighedParticles[P], sender: ActorRef): StateData[P] = {
    val newState = {
      val updatedEGen = helper.addWeightedParticles(weighed, state.evolvingGeneration)
      state.updateEvolvingGeneration(updatedEGen)
    }

    childActors.reporter ! StatusReport(
      NewWeighed(getters.getNumParticles(weighed)),
      newState.evolvingGeneration,
      config
    )

    newState
  }

  def doGenerationFlush(workingData: StateData[P]) {
    childActors.router ! Broadcast(Abort)
    childActors.flusher ! workingData
  }

  def allocateWorkerTask(workingData: StateData[P], worker: ActorRef) {
    val eGen = workingData.evolvingGeneration

    if(eGen.dueWeighing.size > 0)
      worker ! WeighJob.buildFrom(eGen)
    else
      worker ! GenerateParticlesFrom(eGen.previousGen, config)
  }

  def doMixing(state: StateData[P]) {
    val payload: Option[ScoredParticles[P]] = helper.buildMixPayload(
      state.evolvingGeneration,
      config)

    payload.foreach { message =>
      childActors.broadcaster ! MixPayload(message)
    }
  }

  def terminate(state: StateData[P]) {
    childActors.router ! Abort
    childActors.reporter ! state.evolvingGeneration.previousGen
  }

  def startNewGeneration(state: StateData[P]) {
    childActors.router ! Broadcast(GenerateParticlesFrom(
      state.evolvingGeneration.previousGen,
      config))
    childActors.reporter ! state.evolvingGeneration.previousGen
  }

  def sendResultToClient(state: StateData[P]) {

  }
 }
