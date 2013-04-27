package sampler.run.akka

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import sampler.run.Job
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.Props
import scala.language.existentials
import akka.actor.ActorSystem
import sampler.run.Aborter
import sampler.run.UserInitiatedAbortException
import scala.util.Try
import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future

case class Request(job: Job[_], requestor: ActorRef, jobID: Long)
case class WorkAvailable()
case class AbortAll()
case class Delegate(request: Request, worker: ActorRef)

class Master extends Actor with ActorLogging {
	case class BroadcastWorkAvailable()
	
	val workerMgr: ActorRef = context.actorOf(Props[Broadcaster])
	
	val requestQ = collection.mutable.Queue.empty[Request]
	val jobIDIterator = Iterator.iterate(0)(_ + 1)
	
	import context.dispatcher
	context.system.scheduler.schedule(1.seconds, 5.second, self, BroadcastWorkAvailable)
	
	def receive = {
		case job: Job[_] => 
			val requestor = sender
			val jobID = jobIDIterator.next
  		  	val newReq = Request(job, requestor, jobID)
  		  	requestQ += newReq 
  		  	log.info("New job request enqueued, id {}, |Q|={}", jobID, requestQ.size)
		case BroadcastWorkAvailable =>
			if(!requestQ.isEmpty) workerMgr ! Broadcast(WorkAvailable)
		case WorkerIdle =>
			log.info("Idle msg from {}",sender)
			val worker = sender
			if(!requestQ.isEmpty) 
				context.actorOf(Props[RequestSupervisor]) ! Delegate(requestQ.dequeue, worker)
		case AbortAll =>
			//Not all children are Monitors, but non-Monitors 
			//should just ignore the Abort message
			//TODO Use ActorSelection
			log.info("Aborting all work")
			context.children.foreach(_ ! Abort)
			requestQ.clear
	}
}