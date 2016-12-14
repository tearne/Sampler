package sampler.abc.actor.root.state.task

import akka.actor.ActorRef
import sampler.abc.actor.root.state.task.egen.EvolvingGeneration
import sampler.abc.{ABCConfig, Population}

trait Task[P] {
  val config: ABCConfig
  val client: ActorRef

  def updateEvolvingGeneration(eGen: EvolvingGeneration[P]) =
    RunningTask(config, client, eGen)

  def shouldFlush: Boolean
  def shouldTerminate: Boolean
}

case class ResumingTask[P](
    config: ABCConfig,
    client: ActorRef,
    initialPopulation: Population[P]
  ) extends Task[P] {

  def shouldFlush = false
  def shouldTerminate = false
}

case class RunningTask[P](
    config: ABCConfig,
    client: ActorRef,
    evolvingGeneration: EvolvingGeneration[P]
  ) extends Task[P] {

  def shouldFlush =
    evolvingGeneration.weighed.size >= config.numParticles

  def shouldTerminate = {
    evolvingGeneration.previousGen.iteration >= config.numGenerations - 1 &&
      config.terminateAtTargetGen
  }

  //TODO empty weighing buffer?
}
