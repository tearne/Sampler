package sampler.abc.refactor

import akka.actor.ActorRef
import sampler.abc.ABCConfig
import sampler.abc.actor.main.EvolvingGeneration

case class StateData[P](
    config: ABCConfig,
    client: ActorRef,
    evolvingGeneration: EvolvingGeneration[P]){

  def updateEvolvingGeneration(eGen: EvolvingGeneration[P]) =
    copy(evolvingGeneration = eGen)

  def dueToFlush: Boolean =
    evolvingGeneration.weighed.size >= config.numParticles

  def dueToTerminate: Boolean = {
    evolvingGeneration.previousGen.iteration >=
      config.numGenerations &&
      config.terminateAtTargetGen
  }
}
