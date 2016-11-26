package sampler.abc.actor.root

import akka.actor.ActorRef
import sampler.abc.ABCConfig

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
