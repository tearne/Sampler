package sampler.abc.refactor

import akka.actor.Actor
import sampler.abc.{ABCConfig, Generation, Model}
import sampler.abc.actor.main._
import sampler.abc.actor.sub.FlushComplete

class RootActor[P](
                    logic: BusinessLogic[P],
                    config: ABCConfig,
                    model: Model[P]
                  ) extends Actor {
  import Messages._

  //TODO put the mixing timer in a separate child actor
  // which is always firing (for simplicity)

  def receive: Receive = idle()

  def idle(): Receive = {
    case MixNow =>                // Ignored
    case start: Start[P] =>
      val initialState = logic.buildInitialState(start, sender)
      logic.startNewGeneration(initialState)
      context.become(gathering(initialState))
    case _ => //TODO log warning
  }

  def gathering(state: State[P]): Receive = {
    case Failed =>
      logic.workerFailed(state, sender)
    case scored: ScoredParticles[P] =>
      context.become(gathering(logic.addLocallyScoredParticles(state, scored, sender)))
    case mixP: MixPayload[P] =>
      context.become(gathering(logic.addScoredFromMixing(mixP, sender)))
    case weighted: WeighedParticles[P] =>
      val newState = logic.addNewWeighted(state, weighted, sender)
      if(newState.dueToFlush) {
        logic.doGenerationFlush(newState)
        context.become(waitingForFlushComplete(newState))
      } else {
        logic.allocateWork(newState, sender)
      }
    case MixNow => logic.doMixing(state)
    case ReportCompleted => //Do nothing, just don't get upset by the message
    case _ => //TODO log warning
  }

  def waitingForFlushComplete(start: State[P]): Receive ={
    case _: ScoredParticles[P] => // Ignored
    case MixNow =>                // Ignored
    case ReportCompleted =>       // Ignored
    case flushed: FlushCompleteTerminate[P] =>
      logic.terminate(flushed.state)
      context.become(waitingForShutdown(flushed.state.terminate()))
    case flushed: FlushCompleteContinue[P] =>
      logic.startNewGeneration(flushed.state)
      context.become(gathering(flushed.state))
    case _ => //TODO log warning
  }

  def waitingForShutdown(state: State[P]): Receive ={
    case ReportCompleted =>
      logic.sendResultToClient(state)
    case _ => //TODO log warning
  }
}

