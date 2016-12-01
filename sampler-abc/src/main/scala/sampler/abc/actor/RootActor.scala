package sampler.abc.actor

import akka.actor.{Actor, ActorLogging}
import sampler.abc.actor.root.ChildActors
import sampler.abc.actor.root.state.{Dependencies, Idle, State, StateUtil}

class RootActor[P](
    childActors: ChildActors[P],
    util: StateUtil
  ) extends Actor
    with ActorLogging {

  val childRefs = childActors.startup(context)

  def receive = behaviour(
    Idle(Dependencies(
      util, childRefs, self, log
    ))
  )

  def behaviour(state: State): Receive = {
    case message => context.become(
        behaviour(state.evolve(sender, self)(message))
      )
  }
}
