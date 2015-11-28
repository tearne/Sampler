package sampler.abc.actor

import akka.actor.Actor
import akka.actor.ActorLogging
import sampler.abc.actor.algorithm.EvolvingGeneration
import sampler.abc.actor.algorithm.Algorithm

case class FlushComplete[P](eGeneration: EvolvingGeneration[P])

class FlushingActor[P](algorithm: Algorithm) extends Actor with ActorLogging {
	//TODO how to use work dispatcher?
	
	def receive = {
		case eGen: EvolvingGeneration[P] => 
			val newEvolvingGeneration = algorithm.flushGeneration(eGen)
			sender ! FlushComplete(newEvolvingGeneration)
		case msg => log.error("Unexepected message ': {}", msg.getClass)
	}
}