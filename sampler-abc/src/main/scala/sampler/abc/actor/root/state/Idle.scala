package sampler.abc.actor.root.state

import akka.actor.ActorRef
import sampler.abc.actor.message.{MixNow, Start}
import sampler.abc.actor.root.state.task.{ResumingTask, RunningTask}

case class Idle[P](
    dependencies: Dependencies
  ) extends State {
    import dependencies._

    def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, State] = {
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
