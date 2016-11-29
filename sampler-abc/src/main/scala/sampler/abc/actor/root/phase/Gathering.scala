package sampler.abc.actor.root.phase

import akka.actor.ActorRef
import sampler.abc.actor.message._
import sampler.abc.actor.root.phase.task.RunningTask

/**
  * Created by ubuntu on 11/29/16.
  */
case class Gathering[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningPhase[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, Phase] {
    case ReportCompleted => ignore
    case Failed =>
      val newTask = logic.reallocateWorkAfterFailure(task, sender)
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
    case other => ignoreUnexpected(other)
  }
}
