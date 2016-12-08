package sampler.abc.actor.children.refactor

import akka.actor.ActorRef

trait State {
  def evolve(actorRef: ActorRef, self: ActorRef): PartialFunction[Any, State]
}
