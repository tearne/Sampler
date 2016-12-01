package sampler.abc.actor.root.state

import akka.actor.ActorRef
import sampler.abc.actor.message.ReportCompleted
import sampler.abc.actor.root.state.task.RunningTask

case class Terminating[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningState[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, State] = {
    case ReportCompleted =>
      logic.sendResultToClient(task)
      this
    case other => reportAndIgnoreUnexpected(other)
  }
}
