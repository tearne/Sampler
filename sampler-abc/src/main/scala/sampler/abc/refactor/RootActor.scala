package sampler.abc.refactor

import akka.actor.{Actor, ActorLogging, ActorRef}
import sampler.abc.{ABCConfig, Model}
import sampler.abc.actor.main._
import sampler.abc.actor.sub.FlushComplete

import scala.concurrent.{Await, Future}

class RootActor[P](
    childActors: ChildActors[P],
    logic: BusinessLogic[P],
    config: ABCConfig)
      extends Actor
      with ActorLogging {

  childActors.startup(context)
  val childRefs = childActors.resolve(context)

  def receive: Receive = idle()

  def idle(): Receive = {
    case MixNow =>  // Ignored
    case start: Start[P] =>
      val initialState = logic.buildInitialworkingData(start, sender)
      logic.startNewGeneration(initialState, childRefs)
      context.become(gathering(initialState))
    case other => log.warning("Unexpected Message in idle state: {}", other)
  }

  def gathering(state: StateData[P]): Receive = {
    case ReportCompleted => // Ignored
    case Failed =>
      logic.workerFailed(state, sender)
    case scored: ScoredParticles[P] =>
      context.become(gathering(logic.addLocallyScoredParticles(state, scored, sender, childRefs)))
    case mixP: MixPayload[P] =>
      context.become(gathering(logic.addScoredFromMixing(mixP, state, sender, childRefs)))
    case weighted: WeighedParticles[P] =>
      val newState = logic.addNewWeighted(state, weighted, sender, childRefs)
      if(newState.dueToFlush) {
        logic.doGenerationFlush(newState, childRefs)
        context.become(waitingForFlushComplete(newState))
      }
      else logic.allocateWorkerTask(newState, sender)
    case MixNow => logic.doMixing(state, childRefs)
    case other => log.warning("Unexpected Message in gathering state: {}", other)
  }

  def waitingForFlushComplete(state: StateData[P]): Receive ={
    case _: ScoredParticles[P] => // Ignored
    case MixNow =>                // Ignored
    case ReportCompleted =>       // Ignored
    case flushed: FlushComplete[P] =>
      val newState = logic.newFlushedGeneration(flushed, state, childRefs)
      if(state.dueToTerminate) {
        logic.terminate(newState, childRefs)
        context.become(waitingForShutdown(newState))
      } else {
        logic.startNewGeneration(newState, childRefs)
        context.become(gathering(newState))
      }
    case other => log.warning("Unexpected Message while waiting for flush to complete: {}", other)
  }

  def waitingForShutdown(state: StateData[P]): Receive ={
    case ReportCompleted =>
      logic.sendResultToClient(state)
    case other => log.warning("Unexpected Message while waiting for shutdown: {}", other)
  }
}

