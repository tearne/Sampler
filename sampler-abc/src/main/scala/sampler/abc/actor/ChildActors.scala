package sampler.abc.actor

import akka.actor.{ActorContext, ActorRef, Props}
import akka.routing.FromConfig
import akka.util.Timeout
import sampler.abc.actor.children._
import sampler.abc.actor.children.flushing.GenerationFlusher
import sampler.abc.{ABCConfig, Model, Population}
import sampler.maths.Random

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

trait ChildRefs {
  val reporter, workRouter, broadcaster, flusher: ActorRef
}

class ChildActors[P](
    genFlusher: GenerationFlusher,
    config: ABCConfig,
    model: Model[P],
    reportHandler: Option[Population[P] => Unit],
    random: Random) {

  def startup(context: ActorContext) {

    //TODO create 'val mixTimer' here
    // max frequency 10 seconds?

   context.actorOf(
      Props(classOf[BroadcastActor], config),
      "broadcaster"
    )

    context.actorOf(
      Props[ReceiveActor],
      "receiver"
    )

    context.actorOf(
      FromConfig.props(
        Props(new WorkerActorImpl[P](model, random))
      ),
      "work-router"
    )

    context.actorOf(
      //TODO why do both this and reporter have genFlusher?
      Props(classOf[FlushingActor[P]], genFlusher),
      "flusher"
    )

    context.actorOf(
      Props(classOf[ReportingActor[P]], reportHandler),
      "reporter"
    )
  }

  def resolve(context: ActorContext): ChildRefs = {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val timeout = Timeout(1 second)

    val reportingActorFuture = context.actorSelection("reporter").resolveOne()
    val workRouterActorFuture = context.actorSelection("work-router").resolveOne()
    val flusherActorFuture = context.actorSelection("flusher").resolveOne()
    val broadcasterActorFuture = context.actorSelection("broadcaster").resolveOne()

    val futureRefs: Future[ChildRefs] = for{
      reporter0    <- reportingActorFuture
      workRouter0  <- workRouterActorFuture
      flusher0     <- flusherActorFuture
      broadcaster0 <- broadcasterActorFuture
    } yield {
      new ChildRefs{
        val reporter = reporter0
        val workRouter = workRouter0
        val flusher = flusher0
        val broadcaster = broadcaster0
      }
    }

    Await.result(futureRefs, 1 second)
  }
}