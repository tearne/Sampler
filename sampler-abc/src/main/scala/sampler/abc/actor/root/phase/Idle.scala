package sampler.abc.actor.root.phase

import akka.actor.ActorRef
import sampler.abc.actor.message.{MixNow, Start}
import sampler.abc.actor.root.phase.task.{ResumingTask, RunningTask}

case class Idle[P](
    dependencies: Dependencies
  ) extends Phase {
    import dependencies._

    def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, Phase]{
      case MixNow => ignore
      case startMsg: Start[P] =>
        val newTask = logic.initialise(startMsg, childRefs, sender)
        newTask match {
          case runningTask: RunningTask[P] =>
            Gathering(dependencies, runningTask)
          case resumingTask: ResumingTask[P] =>
            Flushing(dependencies, resumingTask)
        }
      case other => reportAndIgnoreUnexpected(other)
    }
  }
