package sampler.abc.actor.root.phase

import akka.actor.ActorRef
import sampler.abc.actor.children._
import sampler.abc.actor.message.{MixPayload, ScoredParticles, Start, WeighedParticles}
import sampler.abc.actor.root.ChildRefs
import sampler.abc.actor.root.phase.task.egen.{EGenUtil, EvolvingGeneration}
import sampler.abc.actor.root.phase.task.{ResumingTask, RunningTask, Task}
import sampler.abc.{ABCConfig, Population, UseModelPrior}
import sampler.io.Logging

import scala.collection.immutable.Queue

class PhaseUtil(
    eGenUtil: EGenUtil,
    config: ABCConfig
  ) extends Logging {

  def initialise[P](
      start: Start[P],
      childRefs: ChildRefs,
      client: ActorRef
    )(
      implicit rootActor: ActorRef
    ): Task[P] = {

    start.initGeneration match {
      case prior: UseModelPrior[P] =>
        val zeroTask = RunningTask(
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
        startNewGeneration(zeroTask, childRefs)
        zeroTask
      case population: Population[P] =>
        val resumeTask = ResumingTask(
          config,
          client,
          population
        )
        startFlush(resumeTask, childRefs)
        resumeTask
    }
  }

  def addLocallyScoredParticles[P](
      state: RunningTask[P],
      scored: ScoredParticles[P],
      worker: ActorRef,
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ): RunningTask[P] = {

    val newTask = {
      val updatedEGen = eGenUtil.filterAndQueueUnweighedParticles(
        scored,
        state.evolvingGeneration
      )
      state.updateEvolvingGeneration(updatedEGen)
    }

    childRefs.reporter ! StatusReport(
      NewScored(scored.seq.size, worker, false),
      newTask.evolvingGeneration,
      config
    )

    allocateWork(newTask, worker)
  }

  def addScoredFromMixing[P](
      mixP: MixPayload[P],
      state: RunningTask[P],
      sender: ActorRef,
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ): RunningTask[P] = {

    val newTask = {
      val newEGen = eGenUtil.filterAndQueueUnweighedParticles(
        mixP.scoredParticles,
        state.evolvingGeneration)
      state.updateEvolvingGeneration(newEGen)
    }

    childRefs.reporter ! StatusReport(
      NewScored(mixP.scoredParticles.seq.size, sender, true),
      newTask.evolvingGeneration,
      config
    )

    newTask
  }

  def addWeighted[P](
      state: RunningTask[P],
      weighed: WeighedParticles[P],
      sender: ActorRef,
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ): RunningTask[P] = {

    val newTask = {
      val updatedEGen = eGenUtil.addWeightedParticles(weighed, state.evolvingGeneration)
      state.updateEvolvingGeneration(updatedEGen)
    }

    childRefs.reporter ! StatusReport(
      NewWeighed(weighed.seq.size),
      newTask.evolvingGeneration,
      config
    )

    newTask
  }

  def startFlush[P](
      state: Task[P],
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ) {

    childRefs.workRouter ! Abort
    childRefs.flusher ! state
  }

  def allocateWork[P](
      task: RunningTask[P],
      worker: ActorRef
    )(
      implicit rootActor: ActorRef
    ): RunningTask[P] = {

    val eGen = task.evolvingGeneration

    if (eGen.dueWeighing.size > 0) {
      worker ! WeighJob.buildFrom(eGen)
      //TODO de-uglify
      val updatedEGen = eGen.emptyWeighingBuffer()
      task.copy(
        evolvingGeneration = updatedEGen
      )
    } else {
      worker ! GenerateParticlesFrom(eGen.previousGen, config)
      task
    }
  }

  def doMixing[P](
      task: RunningTask[P],
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ) {

    val payload: Option[ScoredParticles[P]] = eGenUtil.buildMixPayload(
      task.evolvingGeneration,
      config
    )

    payload.foreach { message =>
      childRefs.broadcaster ! MixPayload(message)
    }
  }

  def updateWithFlushedGeneration[P](
      fc: FlushComplete[P],
      task: Task[P],
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ): RunningTask[P] = {

    val newTask = task.updateEvolvingGeneration(fc.eGeneration)

    val completedGen = newTask.evolvingGeneration.previousGen
    childRefs.reporter ! completedGen

    newTask
  }

  def startTermination[P](
      task: RunningTask[P],
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ) {

    childRefs.workRouter ! Abort
  }

  def startNewGeneration[P](
      task: RunningTask[P],
      childRefs: ChildRefs
    )(
      implicit rootActor: ActorRef
    ) {

    val job = GenerateParticlesFrom(
      task.evolvingGeneration.previousGen,
      config)

    childRefs.workRouter ! job

    val report = {
      val evolvingGen = task.evolvingGeneration
      val currentGen = evolvingGen.buildingGeneration
      val currentTol = evolvingGen.currentTolerance

      StatusReport(
        StartingGen(currentGen, currentTol),
        evolvingGen,
        config
      )
    }

    childRefs.reporter ! report
  }

  def sendResultToClient[P](
      task: RunningTask[P]
    )(
      implicit rootActor: ActorRef
    ) {

    task.client ! task.evolvingGeneration.previousGen
  }
}
