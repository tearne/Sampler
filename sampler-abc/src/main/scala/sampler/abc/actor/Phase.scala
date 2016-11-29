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
    case MixNow => this //Ignored
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

//TODO logic tasks, adding to state Vs starting processes, ...

case class Gathering[P](
    dependencies: Dependencies,
    task: RunningTask[P]
  ) extends RunningPhase[P] {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, Phase] {
    case ReportCompleted => this // Ignored
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
      val newTask = logic.addNewWeighted(task, weighted, sender, childRefs)
      if(newTask.shouldFlush) {
        logic.startGenerationFlush(newTask, childRefs)
        Flushing(dependencies, newTask)
      } else {
        logic.allocateWorkerTask(newTask, sender)
        this.copy(task = newTask)
      }
    case MixNow =>
      logic.doMixing(task, childRefs) //TODO conform to startMixing/doMixing
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
    case _: ScoredParticles[P] =>   this // Ignored
    case _: WeighedParticles[P] =>  this // Ignored
    case MixNow =>                  this // Ignored
    case ReportCompleted =>         this // Ignored
    case flushed: FlushComplete[P] =>
      val newTask = logic.addFlushedGeneration(flushed, task, childRefs)
      if(task.shouldTerminate) {
        logic.terminate(newTask, childRefs) //TODO rename
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
      logic.sendResultToClient(task)//(rootActor)
      this
    case other => unexpected(other)
  }
}
