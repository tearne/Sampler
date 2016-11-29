package sampler.abc.actor

import akka.actor.{Actor, ActorLogging}
import sampler.abc.actor.root.BusinessLogic
import sampler.abc.refactor.ChildActors

class RootActor[P](
  childActors: ChildActors[P],
  logic: BusinessLogic
) extends Actor
  with ActorLogging {

  val childRefs = childActors.startup(context)

  def receive = behaviour(
    Idle(Dependencies(
      logic, childRefs, self, log))
  )

  def behaviour(phase: Phase): Receive = {
    case message => context.become(
        behaviour(phase.evolve(sender, self)(message))
      )
  }
}
