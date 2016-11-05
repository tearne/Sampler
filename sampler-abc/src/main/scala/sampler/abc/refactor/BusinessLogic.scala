package sampler.abc.refactor

import akka.actor.ActorRef
import akka.routing.Broadcast
import sampler.abc.ABCConfig
import sampler.abc.actor.main.component.Helper
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.actor.main.{MixPayload, ScoredParticles, Start, WeighedParticles}
import sampler.abc.actor.sub._
import sampler.io.Logging

class BusinessLogic[P](
    helper: Helper,
    config: ABCConfig,
    getters: Getters) extends Logging {

  def buildInitialworkingData(startMsg: Start[P], client: ActorRef): StateData[P] ={
    StateData(
      config,
      client,
      helper.initialiseEvolvingGeneration(startMsg.initGeneration, config)
    )
  }

  def workerFailed(state: StateData[P], worker: ActorRef) {
    warn("Failure in worker, resending job.")
    allocateWorkerTask(state, worker)
  }

  def addLocallyScoredParticles(
      state: StateData[P],
      scored: ScoredParticles[P],
      worker: ActorRef,
      childRefs: ChildRefs): StateData[P] = {

    val newState = {
      val updatedEGen = helper.filterAndQueueUnweighedParticles(
        scored,
        state.evolvingGeneration
      )
      state.updateEvolvingGeneration(updatedEGen)
    }

    // Report on the new workingData
    childRefs.reporter ! StatusReport( //TODO untested
      NewScored(scored.seq.size, worker, false),
      newState.evolvingGeneration,
      config
    )

    allocateWorkerTask(newState, worker)

    newState
  }

  def addScoredFromMixing(
      mixP: MixPayload[P],
      state: StateData[P],
      sender: ActorRef,
      childRefs: ChildRefs): StateData[P] = {
    val newState = {
      val newEGen = helper.filterAndQueueUnweighedParticles(
        mixP.scoredParticles,
        state.evolvingGeneration)
      state.updateEvolvingGeneration(newEGen)
    }

    childRefs.reporter ! StatusReport(//TODO untested
      NewScored(mixP.scoredParticles.seq.size, sender, true),
      newState.evolvingGeneration,
      config
    )

    newState
  }

  def addNewWeighted(
      state: StateData[P],
      weighed: WeighedParticles[P],
      sender: ActorRef,
      childRefs: ChildRefs): StateData[P] = {
    val newState = {
      val updatedEGen = helper.addWeightedParticles(weighed, state.evolvingGeneration)
      state.updateEvolvingGeneration(updatedEGen)
    }

    childRefs.reporter ! StatusReport(
      NewWeighed(getters.getNumParticles(weighed)),
      newState.evolvingGeneration,
      config
    )

    newState
  }

  def doGenerationFlush(
      workingData: StateData[P],
      childRefs: ChildRefs) {
    childRefs.workRouter ! Broadcast(Abort)
    childRefs.flusher ! workingData
  }

  def allocateWorkerTask(workingData: StateData[P], worker: ActorRef) {
    val eGen = workingData.evolvingGeneration

    if(eGen.dueWeighing.size > 0)
      worker ! WeighJob.buildFrom(eGen)
    else
      worker ! GenerateParticlesFrom(eGen.previousGen, config)
  }

  def doMixing(state: StateData[P], childRefs: ChildRefs) {
    val payload: Option[ScoredParticles[P]] = helper.buildMixPayload(
      state.evolvingGeneration,
      config)

    payload.foreach { message =>
      childRefs.broadcaster ! MixPayload(message)
    }
  }

  def newFlushedGeneration(
      fc: FlushComplete[P],
      state: StateData[P],
      childRefs: ChildRefs): StateData[P] = {
    val newState = state.updateEvolvingGeneration(fc.eGeneration)

    //TODO don't like having to do newState.evolvingGen.previous all th etime

    childRefs.reporter ! StatusReport(
      FinishGen(
        newState.evolvingGeneration.previousGen.iteration,
        newState.evolvingGeneration.currentTolerance),
      newState.evolvingGeneration,
      config
    )

    newState
  }

  def terminate(state: StateData[P], childRefs: ChildRefs) {
    childRefs.workRouter ! Abort
    childRefs.reporter ! state.evolvingGeneration.previousGen
  }

  def startNewGeneration(state: StateData[P], childRefs: ChildRefs) {
    childRefs.workRouter ! Broadcast(GenerateParticlesFrom(
      state.evolvingGeneration.previousGen,
      config))

    val report = {
      val evolvingGen = state.evolvingGeneration
      val prevItNum = evolvingGen.previousGen.iteration
      val currentTol = evolvingGen.buildingGeneration

      StatusReport(
        FinishGen(prevItNum, currentTol),
        evolvingGen,
        config
      )
    }

    childRefs.reporter ! report

  }

  def sendResultToClient(state: StateData[P]) {
    state.client ! state.evolvingGeneration.previousGen
  }
 }
