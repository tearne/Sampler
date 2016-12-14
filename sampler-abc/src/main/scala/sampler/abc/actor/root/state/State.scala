package sampler.abc.actor.root.state

import akka.actor.ActorRef
import sampler.abc.actor.children.FlushComplete
import sampler.abc.actor.message._
import sampler.abc.actor.root._
import sampler.abc.actor.root.state.task.{ResumingTask, RunningTask, Task}


trait State{
  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, State]
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

trait RunningState[P] extends State {
  def task: Task[P]
}








