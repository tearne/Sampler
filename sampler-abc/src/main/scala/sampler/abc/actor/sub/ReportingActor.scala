package sampler.abc.actor.sub

import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.actorRef2Scala
import sampler.abc.actor.main.ReportCompleted
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.config.ABCConfig
import akka.actor.ActorRef
import sampler.abc.Population
import java.math.MathContext

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
	def getTxt = s"Done generation $num, acceptance = ,new tolerance = $tol"
}

case class StatusReport[P](delta: StatusDelta, eGen: EvolvingGeneration[P], config: ABCConfig){
	def getTxt = {
		val due = "|SQ|="+eGen.dueWeighing.size
		val acc = "WAc="+StatusReport.twoSigFig(eGen.weighed.acceptanceRatio)
		val par = "|W|="+eGen.weighed.size+"/"+config.job.numParticles
		val gen = s"G:${eGen.previousGen.iteration}/${config.job.numGenerations}"
		s"($gen, $acc, $par, $due) ${delta.getTxt}"
	}
}
object StatusReport{
  val mc = new MathContext(2)
  def twoSigFig(d: Double) = BigDecimal(d, mc).doubleValue
}

class ReportingActor[P](handler: Option[Population[P] => Unit]) extends Actor with ActorLogging {
	def receive = {
		case gen: Population[P] =>
			handler.foreach(_.apply(gen))
			sender ! ReportCompleted
		case status: StatusReport[P] => 
			log.info(status.getTxt)
		case msg => 
			log.error("Unexepected message ': {}", msg.getClass)
	}
}

