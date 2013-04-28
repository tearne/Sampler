package sampler.run.akka

import akka.actor.Actor
import akka.actor.ActorRef
import akka.cluster.Cluster
import akka.cluster.ClusterEvent.UnreachableMember
import akka.actor.ActorLogging
import sampler.run.Aborter
import scala.concurrent.Future
import scala.util.Try
import akka.pattern.pipe
import akka.actor.ActorSystem
import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scala.util.{Success, Failure}
import org.jboss.netty.logging.Slf4JLogger
import org.jboss.netty.logging.Slf4JLoggerFactory
import org.slf4j.LoggerFactory

case class Abort()
case class StatusRequest()
case class WorkerBusy()
case class WorkerIdle()
case class WorkConfirmed()
case class WorkRejected()

object WorkerApp extends App with HostnameSetup{
	if(args.size == 1){
		System.setProperty("akka.remote.netty.port", args(0))
		ConfigFactory.invalidateCaches()
	}
	val system = ActorSystem("ClusterSystem")
	system.actorOf(Props[Worker], name = "worker")
}

/*
 * Keep the worker simple.  It just responds to status requests
 * and switches between idle and busy states, with the option 
 * of aborting work
 */
class Worker extends Actor with ActorLogging{
	case class Done()
	
	def receive = idle
	
	def idle: Receive = {
		case StatusRequest => sender ! WorkerIdle
		case WorkAvailable => sender ! WorkerIdle
		case request: Request =>
			log.info("Got request "+request.jobID)
			val sndr = sender
			val me = self
			val aborter = new Aborter
			context.become(busy(aborter))
			import context._
			Future{
				val result = Try(request.job.run(aborter))
				me ! Done
				sndr ! result
			}
			sndr ! WorkConfirmed
	}
	
	def busy(aborter: Aborter): Receive = {
		case StatusRequest => sender ! WorkerBusy
		case Abort => 
			log.info("Aborting")
			aborter.abort
			context.become(idle)
		case Done => 
			log.info("Becoming idle")
			context.become(idle)
		case Request => log.warning("Received request when busy, ignoring")
		case WorkAvailable => //Ignore
		case msg => log.warning("unknown msg "+msg.toString)
	}
}