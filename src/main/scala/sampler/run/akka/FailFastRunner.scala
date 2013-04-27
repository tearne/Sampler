package sampler.run.akka

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import akka.actor.ActorDSL.Act
import akka.actor.ActorDSL.actor
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.pattern.ask
import akka.util.Timeout
import sampler.run.Job
import sampler.run.JobRunner
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef

class FailFastRunner(val system: ActorSystem) extends JobRunner{
	implicit val timeout = Timeout(5.minutes)
	import system.dispatcher
	
	def apply[T](jobs: Seq[Job[T]]): Seq[Try[T]] = {
		val ff = system.actorOf(Props[FailFastActor])
		Await.result((ff ? jobs).mapTo[Seq[Try[T]]], timeout.duration)
	}
}

class FailFastActor extends Actor with ActorLogging{
	val results = collection.mutable.Buffer[Try[Any]]()
	val master = context.actorOf(Props[Master], name = "master")
	
	def receive = startup
	
	def startup: Receive = {
		case jobs: Seq[Job[_]] => 
			val client = sender
			val numJobs = jobs.size
			jobs.foreach(master ! _)
			context.become(running(client, numJobs))
	}
	
	def running(client: ActorRef, numJobs: Int): Receive = {
		case s: Success[_] => 
			results += s
			if(results.size == numJobs) { 
				client ! results
				self ! PoisonPill
			}
		case f: Failure[_] => 
			log.warning("Sending abort to "+master)
			master ! AbortAll
			results += f
			client ! results.toSeq
			self ! PoisonPill
	}
}