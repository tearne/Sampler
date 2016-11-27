package sampler.abc.actor.root

import akka.actor.ActorRef
import sampler.abc.{ABCConfig, Population, UseModelPrior}
import sampler.abc.actor.children._
import sampler.abc.refactor.ChildRefs
import sampler.io.Logging

import scala.collection.immutable.Queue

class BusinessLogic(
    helper: Helper,
    config: ABCConfig,
    getters: Getters) extends Logging {

  def initialise[P](
      start: Start[P],
      config: ABCConfig,
      childRefs: ChildRefs,
      client: ActorRef
  )(
      implicit rootActor: ActorRef
  ): State[P] = {

    start.initGeneration match {
      case prior: UseModelPrior[P] =>
        val zeroState = RunningState(
          config,
          client,
          EvolvingGeneration(
            prior.tolerance,
            prior,
            ScoredParticles.empty,
            WeighedParticles.empty,
            Queue.empty[Long]
          )
        )
        startNewGeneration(zeroState, childRefs)
        zeroState
      case population: Population[P] =>
        val resumeState = ResumingState(
          config,
          client,
          population
        )
        startGenerationFlush(resumeState, childRefs)
        resumeState
    }
  }

  //TODO moving this
//  def buildInitialworkingData(startMsg: Start[P], client: ActorRef): StateData[P] = {
//    StateData(
//      config,
//      client,
//      helper.initialiseEvolvingGeneration(startMsg.initGeneration, config)
//    )
//  }

  def workerFailed[P](state: RunningState[P], worker: ActorRef)(implicit rootActor: ActorRef) {
    warn("Failure in worker, resending job.")
    allocateWorkerTask(state, worker)
  }

  def addLocallyScoredParticles[P](
      state: RunningState[P],
      scored: ScoredParticles[P],
      worker: ActorRef,
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef): RunningState[P] = {

    val newState = {
      val updatedEGen = helper.filterAndQueueUnweighedParticles(
        scored,
        state.evolvingGeneration
      )
      state.updateEvolvingGeneration(updatedEGen)
    }

    // Report on the new workingData
    childRefs.reporter ! StatusReport(//TODO untested
      NewScored(scored.seq.size, worker, false),
      newState.evolvingGeneration,
      config
    )

    allocateWorkerTask(newState, worker)

    newState
  }

  def addScoredFromMixing[P](
      mixP: MixPayload[P],
      state: RunningState[P],
      sender: ActorRef,
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef): RunningState[P] = {
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

  def addNewWeighted[P](
      state: RunningState[P],
      weighed: WeighedParticles[P],
      sender: ActorRef,
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef): RunningState[P] = {
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

  def startGenerationFlush[P](
      state: State[P],
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef) {
    childRefs.workRouter ! Abort
    childRefs.flusher ! state
  }

  def allocateWorkerTask[P](
      workingData: RunningState[P],
      worker: ActorRef)(
      implicit rootActor: ActorRef) {

    val eGen = workingData.evolvingGeneration

    if (eGen.dueWeighing.size > 0)
      worker ! WeighJob.buildFrom(eGen)
    else
      worker ! GenerateParticlesFrom(eGen.previousGen, config)
  }

  def doMixing[P](
      state: RunningState[P],
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef) {

    val payload: Option[ScoredParticles[P]] = helper.buildMixPayload(
      state.evolvingGeneration,
      config)

    payload.foreach { message =>
      childRefs.broadcaster ! MixPayload(message)
    }
  }

  def newFlushedGeneration[P](
      fc: FlushComplete[P],
      state: State[P],
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef): RunningState[P] = {

    val newState = state.updateEvolvingGeneration(fc.eGeneration)

    newState
  }

  def terminate[P](
      state: RunningState[P],
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef) {

    childRefs.workRouter ! Abort
    childRefs.reporter ! state.evolvingGeneration.previousGen
  }

  def startNewGeneration[P](
      state: RunningState[P],
      childRefs: ChildRefs)(
      implicit rootActor: ActorRef) {

    val job = GenerateParticlesFrom(
      state.evolvingGeneration.previousGen,
      config)

    childRefs.workRouter ! job

    val report = {
      val evolvingGen = state.evolvingGeneration
      val prevItNum = evolvingGen.previousGen.iteration
      val currentTol = evolvingGen.currentTolerance

      StatusReport(
        StartingGen(prevItNum, currentTol),
        evolvingGen,
        config
      )
    }

    childRefs.reporter ! report
  }

  def sendResultToClient[P](state: RunningState[P])(implicit rootActor: ActorRef) {
    state.client ! state.evolvingGeneration.previousGen
  }
}
