package sampler.abc.actor

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.event.LoggingAdapter
import sampler.abc.actor.children.FlushComplete
import sampler.abc.actor.root._
import sampler.abc.refactor.{ChildActors, ChildRefs}

/*
Experiment inspired by
https://alexn.org/blog/2015/12/15/avoid-javaisms-code-smell.html
 */

case class Dependencies(
  logic: BusinessLogic,
  childRefs: ChildRefs,
  rootActor: ActorRef,
  log: LoggingAdapter
)

trait Phase{
  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase]
  def dependencies: Dependencies

  val ignore = this

  implicit val rootActor0 = dependencies.rootActor

  def unexpected(msg: Any) = {
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

case class Idle[P](dependencies: Dependencies) extends Phase {
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
    case other => unexpected(other)
  }
}

case class Gathering[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningPhase[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, Phase] {
    case ReportCompleted => ignore
    case Failed =>
      logic.reallocateWorkAfterFailure(task, sender)
      this
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
    case other => unexpected(other)
  }
}

case class Flushing[P](
    dependencies: Dependencies,
  task: Task[P]
  ) extends RunningPhase[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase] = {
    case _: ScoredParticles[P] =>   ignore
    case _: WeighedParticles[P] =>  ignore
    case MixNow =>                  ignore
    case ReportCompleted =>         ignore
    case flushed: FlushComplete[P] =>
      val newTask = logic.updateWithFlushedGeneration(flushed, task, childRefs)
      if(task.shouldTerminate) {
        logic.startTermination(newTask, childRefs)
        ShuttingDown(dependencies, newTask)
      } else {
        logic.startNewGeneration(newTask, childRefs)
        Gathering(dependencies, newTask)
      }
    case other => unexpected(other)
  }
}

case class ShuttingDown[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningPhase[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase] = {
    case ReportCompleted =>
      logic.sendResultToClient(task)
      this
    case other => unexpected(other)
  }
}
