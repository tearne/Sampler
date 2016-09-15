package sampler.abc.refactor

import akka.actor.Actor
import sampler.abc.{ABCConfig, Generation, Model}
import sampler.abc.actor.main._
import sampler.abc.actor.sub.FlushComplete

object Messages {
  case class Start[P](initGen: Generation[P])
}

class RootActor[P](
                    logic: BusinessLogic[P],
                    config: ABCConfig,
                    model: Model[P]
                  ) extends Actor {
  import Messages._

  //TODO put the mixing timer in a separate actor

  def receive: Receive = idle()

  def idle(): Receive = {
    case s: Start[P] =>
      context.become(gathering(logic.start(s, sender)))
    case _ => //TODO log warning
  }

  def gathering(state: State[P]): Receive = {
    case Failed =>
      logic.workerFailed(state, sender)
    case scored: ScoredParticles[P] =>
      context.become(gathering(logic.addNewLocalScored(state, scored, sender)))
    case weighted: WeighedParticles[P] =>
      val newState = logic.addNewWeighted(state, weighted, sender)
      if(newState.dueToFlush) context.become(flushing(newState))
    case MixNow =>
      state.doMixing()
    case mixP: MixPayload[P] =>
      context.become(gathering(state.addScoredFromMixPayload(mixP, sender)))
    case ReportCompleted => //Do nothing, just don't get upset by the message
    case _ => //TODO log warning
  }

  def flushing(start: State): Receive ={
    case _: ScoredParticles[P] => // Ignored
    case MixNow =>                // Ignored
    case ReportCompleted =>       // Ignored
    case fc: FlushCompleteTerminate[P] =>
      //Stop child actors, send to reporter
      context.become(waitingForShutdown(fc.state.terminate()))
    case fc: FlushCompleteContinue[P] =>
      //
      context.become(gathering(fc.state.startNextGeneration()))
    case _ => //TODO log warning
  }

  def waitingForShutdown(state: BusinessLogic): Receive ={
    case ReportCompleted =>
      // Send final results to clinet
    case _ => //TODO log warning
  }
}

