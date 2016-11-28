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
    dependencies.log.warning("Unexpected message encountered in {}: [{} ...]", this.getClass, msg.toString.take(20))
    this
  }
}

trait Running[P] extends Phase {
  def state: State[P]
}

case class Idle[P](dependencies: Dependencies) extends Phase {
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, Phase]{
    case MixNow => this //Ignored
    case startMsg: Start[P] =>
      val newState = logic.initialise(startMsg, childRefs, sender)
      newState match {
        case runningState: RunningState[P] =>
          Gathering(dependencies, runningState)
        case resumingState: ResumingState[P] =>
          Flushing(dependencies, resumingState)
      }
    case other => unexpected(other)
  }
}

//TODO logic tasks, adding, starting, ...

case class Gathering[P](dependencies: Dependencies, state: RunningState[P]) extends Running[P]{
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef) = PartialFunction[Any, Phase] {
    case ReportCompleted => this // Ignored
    case Failed =>
      logic.workerFailed(state, sender)
      this
    case scored: ScoredParticles[P] =>
      val newState = logic.addLocallyScoredParticles(state, scored, sender, childRefs)
      this.copy(state = newState)
    case mixP: MixPayload[P] =>
      val newState = logic.addScoredFromMixing(mixP, state, sender, childRefs)
      this.copy(state = newState)
    case weighted: WeighedParticles[P] =>
      val newState = logic.addNewWeighted(state, weighted, sender, childRefs)
      if(newState.shouldFlush) {
        logic.startGenerationFlush(newState, childRefs)
        Flushing(dependencies, newState)
      } else {
        logic.allocateWorkerTask(newState, sender)
        this.copy(state = newState)
      }
    case MixNow =>
      logic.doMixing(state, childRefs) //TODO conform to startMixing/doMixing
      this
    case other => unexpected(other)
  }
}

case class Flushing[P](dependencies: Dependencies, state: State[P]) extends Running[P]{
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase] = {
    case _: ScoredParticles[P] => this // Ignored
    case MixNow =>                this // Ignored
    case ReportCompleted =>       this // Ignored
    case flushed: FlushComplete[P] =>
      val newState = logic.newFlushedGeneration(flushed, state, childRefs)
      if(state.shouldTerminate) {
        logic.terminate(newState, childRefs) //TODO rename
        ShuttingDown(dependencies, newState)
      } else {
        logic.startNewGeneration(newState, childRefs)
        Gathering(dependencies, newState)
      }
    case other => unexpected(other)
  }
}

case class ShuttingDown[P](dependencies: Dependencies, state: RunningState[P]) extends Running[P]{
  import dependencies._

  def evolve(sender: ActorRef, rootActor: ActorRef): PartialFunction[Any, Phase] = {
    case ReportCompleted =>
      logic.sendResultToClient(state)//(rootActor)
      this
    case other => unexpected(other)
  }
}

class StateFreeRootActor[P](childActors: ChildActors[P], logic: BusinessLogic)
    extends Actor with ActorLogging {

  val childRefs = {
    childActors.startup(context)
    childActors.resolve(context) //TODO roll into one?
  }

  def receive = behaviour(
    Idle(Dependencies(logic, childRefs, self, log))
  )

  def behaviour(phase: Phase): Receive = {
    case message =>
      val newPhase = phase.evolve(sender, self)(message)
      context.become(behaviour(newPhase))
      log.info("Became "+newPhase.toString.take(100))
  }
}
