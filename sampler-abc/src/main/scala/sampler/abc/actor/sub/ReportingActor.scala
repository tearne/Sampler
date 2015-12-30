package sampler.abc.actor.sub

import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.actorRef2Scala
import sampler.abc.actor.main.ReportCompleted
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.config.ABCConfig
import akka.actor.ActorRef
import sampler.abc.Generation

sealed trait StatusDelta{
	def getTxt(): String
}
case class NewScored(num: Int, sender: ActorRef, fromRemoteActor: Boolean) extends StatusDelta{
	def getTxt = {
		s"+$num scored from${if(fromRemoteActor) " remote " else " "}$sender"
	}
}
case class NewWeighed(num: Int) extends StatusDelta {
	def getTxt = s"+$num weighed"
}
case class FinishGen(num: Int, tol: Double) extends StatusDelta {
	def getTxt = s"Done generation $num, new tolerance = $tol"
}

case class StatusReport[P](delta: StatusDelta, eGen: EvolvingGeneration[P], config: ABCConfig){
	def getTxt = {
		val due = "|SQ|="+eGen.dueWeighing.size
		val par = "|W|="+eGen.weighed.size+"/"+config.job.numParticles
		val gen = s"G:${eGen.previousGen.iteration}/${config.job.numGenerations}"
		s"($gen, $par, $due) ${delta.getTxt}"
	}
}

class ReportingActor[P](handler: Option[Generation[P] => Unit]) extends Actor with ActorLogging {
	def receive = {
		case gen: Generation[P] =>
			handler.foreach(_.apply(gen))
			sender ! ReportCompleted
		case status: StatusReport[P] => 
			log.info(status.getTxt)
		case msg => 
			log.error("Unexepected message ': {}", msg.getClass)
	}
}

