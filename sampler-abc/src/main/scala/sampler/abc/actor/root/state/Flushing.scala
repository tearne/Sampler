package sampler.abc.actor.root.state

import akka.actor.ActorRef
import sampler.abc.actor.children.FlushComplete
import sampler.abc.actor.message.{MixNow, ReportCompleted, ScoredParticles, WeighedParticles}
import sampler.abc.actor.root.state.task.Task

case class Flushing[P](
    dependencies: Dependencies,
    task: Task[P]
  ) extends RunningState[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, State] = {
    case _: ScoredParticles[P] =>   ignore
    case _: WeighedParticles[P] =>  ignore
    case MixNow =>                  ignore
    case ReportCompleted =>         ignore
    case flushed: FlushComplete[P] =>
      val newTask = logic.updateWithFlushedGeneration(flushed, task, childRefs)
      if(task.shouldTerminate) {
        logic.startTermination(newTask, childRefs)
        Terminating(dependencies, newTask)
      } else {
        logic.startNewGeneration(newTask, childRefs)
        Gathering(dependencies, newTask)
      }
    case other => reportAndIgnoreUnexpected(other)
  }
}
