package sampler.abc.refactor

import akka.actor.Actor
import sampler.abc.Generation
import sampler.abc.actor.main._
import sampler.abc.actor.sub.FlushComplete

object Messages {
  case class Start[P](initGen: Generation[P])
}

class RootActor[P](businessLogic: BusinessLogic) extends Actor {
  import Messages._

  def receive = idle()

  def idle: Receive = {
    case s: Start[P] =>
      context.become(businessLogic.init(s.initGen))
    case _ => //log warning



  def gathering: Receive = {
    case Failed =>
    case scored: ScoredParticles[P] =>
    case weighted: WeighedParticles[P] =>
    case MixNow =>
    case mixP: MixPayload[P] =>
    case ReportCompleted =>
  }.

  def flushing: Receive ={
    case _: ScoredParticles[P] =>
    case MixNow =>
    case fc: FlushComplete[P] =>
    case ReportCompleted =>
  }

  def waitingForShutdown: Receive ={
    case ReportCompleted =>
  }
}

