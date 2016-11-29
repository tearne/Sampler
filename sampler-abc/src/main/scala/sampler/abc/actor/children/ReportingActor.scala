package sampler.abc.actor.children

import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.actorRef2Scala
import sampler.abc.actor.root.ReportCompleted
import akka.actor.ActorRef
import sampler.abc.Population
import java.math.MathContext

import sampler.abc.ABCConfig
import sampler.abc.actor.root.phase.task.egen.EvolvingGeneration

sealed trait StatusDelta{
	def getMsg(): String
}
case class NewScored(num: Int, sender: ActorRef, fromRemoteActor: Boolean) extends StatusDelta{
	def getMsg = {
		s"+$num scored from${if(fromRemoteActor) " remote " else " "}$sender"
	}
}
case class NewWeighed(num: Int) extends StatusDelta {
	def getMsg = s"+$num weighed"
}
case class StartingGen(num: Int, tol: Double) extends StatusDelta {
	def getMsg = s"Start building gen $num with tolerance = $tol"
}

case class StatusReport[P](delta: StatusDelta, eGen: EvolvingGeneration[P], config: ABCConfig){
	def getTxt = {
		val due = "|SQ|="+eGen.dueWeighing.size
		val acc = s"Acc=${StatusReport.percentage(eGen.weighed.acceptanceRatio)}%"
		val par = "|W|="+eGen.weighed.size+"/"+config.numParticles
		val gen = s"G:${eGen.previousGen.iteration}+/${config.numGenerations}"
		s"($gen, $acc, $par, $due) ${delta.getMsg}"
	}
}
object StatusReport{
  val mc = new MathContext(2)
  def percentage(d: Double) = BigDecimal(d*100, mc).intValue
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

