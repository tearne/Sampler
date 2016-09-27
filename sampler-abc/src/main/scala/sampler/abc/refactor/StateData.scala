package sampler.abc.refactor

import akka.actor.ActorRef
import sampler.abc.actor.main.EvolvingGeneration

case class StateData[P](
    requiredNumParticles: Int,
    client: ActorRef,
    evolvingGeneration: EvolvingGeneration[P]){

  def updateEvolvingGeneration(eGen: EvolvingGeneration[P]) = copy(evolvingGeneration = eGen)
  def dueToFlush: Boolean = evolvingGeneration.weighed.size >= requiredNumParticles
}
