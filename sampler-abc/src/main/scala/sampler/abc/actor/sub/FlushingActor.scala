package sampler.abc.actor.sub

import akka.actor.Actor
import akka.actor.ActorLogging
import sampler.abc.actor.main.EvolvingGeneration
import akka.actor.actorRef2Scala
import sampler.abc.actor.sub.flushing.GenerationFlusher

case class FlushComplete[P](eGeneration: EvolvingGeneration[P])

class FlushingActor[P](generationFlusher: GenerationFlusher) extends Actor with ActorLogging {
	
	def receive = {
		case eGen: EvolvingGeneration[P] => 
			sender ! FlushComplete(generationFlusher(eGen))
		case msg => 
			log.error("Unexepected message ': {}", msg.getClass)
	}
}