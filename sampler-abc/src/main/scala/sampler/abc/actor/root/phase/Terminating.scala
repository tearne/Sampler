package sampler.abc.actor.root.phase

import akka.actor.ActorRef
import sampler.abc.actor.message.ReportCompleted
import sampler.abc.actor.root.phase.task.RunningTask

case class Terminating[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningPhase[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase] = {
    case ReportCompleted =>
      logic.sendResultToClient(task)
      this
    case other => reportAndIgnoreUnexpected(other)
  }
}
