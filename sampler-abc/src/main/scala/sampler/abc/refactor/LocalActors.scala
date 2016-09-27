package sampler.abc.refactor

import akka.actor.{ActorContext, Props}
import akka.routing.FromConfig
import sampler.abc.{ABCConfig, Model}
import sampler.abc.actor.sub._
import sampler.abc.actor.sub.flushing.GenerationFlusher
import sampler.maths.Random


class LocalActors[P](
    context: ActorContext,
    genFlusher: GenerationFlusher,
    config: ABCConfig,
    model: Model[P],
    random: Random) {

  //TODO create 'val mixTimer' here
  // max frequency 10 seconds?

  val broadcaster = context.actorOf(
    Props(classOf[BroadcastActor], config),
    "broadcaster"
  )

  val receiver = context.actorOf(
    Props[ReceiveActor],
    "receiver"
  )

  val router = context.actorOf(
    FromConfig.props(
      Props(new WorkerActorImpl[P](model, random))
    ),
    "work-router"
  )

  val flusher = context.actorOf(
    //TODO why do both this and reporter have genFlusher?
    Props(classOf[FlushingActor[P]], genFlusher),
    "flusher"
  )

  val reporter = context.actorOf(
    Props(classOf[ReportingActor[P]], genFlusher)
  )
}
