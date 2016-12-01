package sampler.abc.actor.root.state

import akka.actor.ActorRef
import sampler.abc.actor.message._
import sampler.abc.actor.root.state.task.RunningTask

case class Gathering[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningState[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, State] {
    case ReportCompleted => ignore
    case Failed =>
      dependencies.log.warning("Failure in worker, resending job.")
      val newTask = logic.allocateWork(task, sender)
      this.copy(task = newTask)
    case scored: ScoredParticles[P] =>
      val newTask = logic.addLocallyScoredParticles(task, scored, sender, childRefs)
      this.copy(task = newTask)
    case mixP: MixPayload[P] =>
      val newTask = logic.addScoredFromMixing(mixP, task, sender, childRefs)
      this.copy(task = newTask)
    case weighted: WeighedParticles[P] =>
      val newTask = logic.addWeighted(task, weighted, sender, childRefs)
      if(newTask.shouldFlush) {
        logic.startFlush(newTask, childRefs)
        Flushing(dependencies, newTask)
      } else {
        logic.allocateWork(newTask, sender)
        this.copy(task = newTask)
      }
    case MixNow =>
      logic.doMixing(task, childRefs)
      this
    case other => reportAndIgnoreUnexpected(other)
  }
}
