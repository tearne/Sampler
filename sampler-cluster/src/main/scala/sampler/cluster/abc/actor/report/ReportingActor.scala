package sampler.cluster.abc.actor.report

import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.ActorRef
import sampler.cluster.abc.state.State
import akka.actor.actorRef2Scala
import sampler.cluster.abc.actor.Report
import sampler.cluster.abc.actor.Start

class ReportingActor[P](algorithmRoot: ActorRef, action: Option[Report[P] => Unit]) extends Actor with ActorLogging {
	def receive = idle
	
	def idle: Receive = {
		case init: State[P] => 
			val client = sender
			algorithmRoot ! Start(init)
			context.become(working(client))
		case msg => log.error("Unexepected message when 'idle' state: {}", msg.getClass)
	}
	
	def working(client: ActorRef): Receive = {
		case report: Report[P] =>
			action.foreach(a => a(report))
			if(report.isFinalReport) client ! report
		case msg => log.error("Unexepected message when 'working': {}", msg.getClass)
	}
}

