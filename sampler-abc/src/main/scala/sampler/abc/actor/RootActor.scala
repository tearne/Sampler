package sampler.abc.refactor

import akka.actor.{Actor, ActorLogging}
import sampler.abc.ABCConfig
import sampler.abc.actor.children.FlushComplete
import sampler.abc.actor.root._

class RootActor[P](
    childActors: ChildActors[P],
    logic: BusinessLogic,
    config: ABCConfig)
      extends Actor
      with ActorLogging {

  childActors.startup(context)
  val childRefs = childActors.resolve(context)

  def receive: Receive = idle()

  def idle(): Receive = {
    case MixNow =>  // Ignored
    case startMsg: Start[P] =>
      logic.initialise(startMsg, config, childRefs, sender) match {
        case zeroState: RunningState[P] =>
          context.become(gathering(zeroState))
        case resumeState: ResumingState[P] =>
          context.become(waitingForFlushComplete(resumeState))
      }

    case other => log.warning("Unexpected Message in idle state: {}", other)
  }

  def gathering(state: RunningState[P]): Receive = {
    case ReportCompleted => // Ignored
    case Failed =>
      logic.workerFailed(state, sender)
    case scored: ScoredParticles[P] =>
      context.become(gathering(logic.addLocallyScoredParticles(state, scored, sender, childRefs)))
    case mixP: MixPayload[P] =>
      context.become(gathering(logic.addScoredFromMixing(mixP, state, sender, childRefs)))
    case weighted: WeighedParticles[P] =>
      val newState = logic.addNewWeighted(state, weighted, sender, childRefs)
      if(newState.shouldFlush) {
        logic.startGenerationFlush(newState, childRefs)
        context.become(waitingForFlushComplete(newState))
      }
      else {
        logic.allocateWorkerTask(newState, sender)
        context.become(gathering(newState))
      }
    case MixNow => logic.doMixing(state, childRefs)
    case other => log.warning("Unexpected Message in gathering state: {}", other)
  }

  def waitingForFlushComplete(state: State[P]): Receive ={
    case _: ScoredParticles[P] => // Ignored
    case MixNow =>                // Ignored
    case ReportCompleted =>       // Ignored
    case flushed: FlushComplete[P] =>
      val newState = logic.newFlushedGeneration(flushed, state, childRefs)
      if(state.shouldTerminate) {
        logic.terminate(newState, childRefs)
        context.become(waitingForShutdown(newState))
      } else {
        logic.startNewGeneration(newState, childRefs)
        context.become(gathering(newState))
      }
    case other => log.warning("Unexpected Message while waiting for flush to complete: {}", other)
  }

  def waitingForShutdown(state: RunningState[P]): Receive ={
    case ReportCompleted =>
      logic.sendResultToClient(state)
    case other => log.warning("Unexpected Message while waiting for shutdown: {}", other)
  }
}

