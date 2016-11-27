package sampler.abc.actor.children

import akka.actor.{Actor, ActorLogging, actorRef2Scala}
import sampler.abc.actor.children.flushing.GenerationFlusher
import sampler.abc.actor.root.{EvolvingGeneration, ResumingState, RunningState}

case class FlushComplete[P](eGeneration: EvolvingGeneration[P])

class FlushingActor[P](generationFlusher: GenerationFlusher) extends Actor with ActorLogging {

  def receive = {
    //TODO test me
    case resuming: ResumingState[P] =>
      sender ! FlushComplete(
        generationFlusher.fromPreExistingPopulation(resuming.initialPopulation)
      )

    case running: RunningState[P] =>
      sender ! FlushComplete(
        generationFlusher.fromEvolvingGen(running.evolvingGeneration)
      )

    case msg =>
      log.error("Unexepected message ': {}", msg.getClass)
  }
}