package sampler.abc.actor.root

import akka.actor.ActorRef
import sampler.abc.{ABCConfig, Population}

trait State[P] {
  val config: ABCConfig
  val client: ActorRef

  def updateEvolvingGeneration(eGen: EvolvingGeneration[P]) =
    RunningState(config, client, eGen)

  def shouldFlush: Boolean
  def shouldTerminate: Boolean
}

/*
Used when resuming from prevous generation data and waiting for the next tolerance
to be come back from the generation flushing process.
 */
case class ResumingState[P](
    config: ABCConfig,
    client: ActorRef,
    initialPopulation: Population[P]
  ) extends State[P] {
  def shouldFlush = false
  def shouldTerminate = false
}

case class RunningState[P](
    config: ABCConfig,
    client: ActorRef,
    evolvingGeneration: EvolvingGeneration[P]
  ) extends State[P] {

  def shouldFlush: Boolean =
    evolvingGeneration.weighed.size >= config.numParticles

  def shouldTerminate: Boolean = {
    evolvingGeneration.previousGen.iteration >= config.numGenerations - 1 &&
      config.terminateAtTargetGen
  }
}
