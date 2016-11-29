package sampler.abc.actor.root

import akka.actor.ActorRef
import sampler.abc.{ABCConfig, Population}

trait Task[P] {
  val config: ABCConfig
  val client: ActorRef

  def updateEvolvingGeneration(eGen: EvolvingGeneration[P]) =
    RunningTask(config, client, eGen)

  def shouldFlush: Boolean
  def shouldTerminate: Boolean
}

/*
Used when resuming from previous generation data and waiting for the next tolerance
to be come back from the generation flushing process.
 */
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
}
