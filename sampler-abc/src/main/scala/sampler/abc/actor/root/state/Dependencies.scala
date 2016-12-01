package sampler.abc.actor.root.state

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import sampler.abc.actor.root.ChildRefs

case class Dependencies(
  logic: StateUtil,
  childRefs: ChildRefs,
  rootActor: ActorRef,
  log: LoggingAdapter
)
