package sampler.abc.actor.root.phase

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import sampler.abc.actor.root.ChildRefs

case class Dependencies(
  logic: PhaseUtil,
  childRefs: ChildRefs,
  rootActor: ActorRef,
  log: LoggingAdapter
)
