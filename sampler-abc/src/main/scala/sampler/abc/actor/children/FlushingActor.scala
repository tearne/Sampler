package sampler.abc.actor.children

import akka.actor.Actor
import akka.actor.ActorLogging
import sampler.abc.actor.root.EvolvingGeneration
import akka.actor.actorRef2Scala
import sampler.abc.actor.children.flushing.GenerationFlusher

case class FlushComplete[P](eGeneration: EvolvingGeneration[P])

class FlushingActor[P](generationFlusher: GenerationFlusher) extends Actor with ActorLogging {
	
	def receive = {
		case eGen: EvolvingGeneration[P] => 
			sender ! FlushComplete(generationFlusher(eGen))
		case msg => 
			log.error("Unexepected message ': {}", msg.getClass)
	}
}