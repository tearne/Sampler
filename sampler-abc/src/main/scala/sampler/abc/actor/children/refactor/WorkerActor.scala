package sampler.abc.actor.children.refactor

import akka.actor.Actor

class WorkerActor() extends Actor {
  def receive = behaviour(
    null//Idle()
  )

  def behaviour(state: State): Receive = {
    case message => context.become(
      behaviour(state.evolve(sender, self)(message))
    )
  }
}
