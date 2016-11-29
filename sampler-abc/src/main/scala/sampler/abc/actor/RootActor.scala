package sampler.abc.actor

import akka.actor.{Actor, ActorLogging}
import sampler.abc.actor.root.phase.{Dependencies, Idle, Phase, PhaseUtil}
import sampler.abc.refactor.ChildActors

class RootActor[P](
  childActors: ChildActors[P],
  logic: PhaseUtil
) extends Actor
  with ActorLogging {

  val childRefs = childActors.startup(context)

  def receive = behaviour(
    Idle(Dependencies(
      logic, childRefs, self, log
    ))
  )

  def behaviour(phase: Phase): Receive = {
    case message => context.become(
        behaviour(phase.evolve(sender, self)(message))
      )
  }
}
