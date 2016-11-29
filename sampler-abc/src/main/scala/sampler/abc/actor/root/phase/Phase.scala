package sampler.abc.actor.root.phase

import akka.actor.ActorRef
import sampler.abc.actor.children.FlushComplete
import sampler.abc.actor.message._
import sampler.abc.actor.root._
import sampler.abc.actor.root.phase.task.{ResumingTask, RunningTask, Task}


trait Phase{
  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase]
  def dependencies: Dependencies

  val ignore = this

  implicit val rootActor0 = dependencies.rootActor

  def reportAndIgnoreUnexpected(msg: Any) = {
    dependencies.log.warning(
      "Unexpected message encountered in {}: {} [...]",
      getClass,
      msg.toString.take(50)
    )
    this
  }
}

trait RunningPhase[P] extends Phase {
  def task: Task[P]
}








