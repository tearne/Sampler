package sampler.cluster.abc.master

import akka.actor.ActorLogging
import akka.actor.Actor
import scala.util.Try
import akka.actor.Props
import sampler.cluster.abc.ABCJob
import akka.actor.ActorRef
import scala.util.Success
import akka.actor.PoisonPill
import scala.util.Failure
import sampler.abc.ABCModel
import sampler.abc.Particle
import sampler.abc.MaxRetryException
import akka.actor.ActorSelection
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await
import sampler.cluster.abc.IndexedJob
import sampler.cluster.abc.AbortJob

class AccumulatorActor(master: ActorRef) extends Actor with ActorLogging{
	val results = collection.mutable.Buffer[Particle[_]]()
				
	def receive = startup
	
	override def postStop(): Unit = {
		log.info("Post stop")
	}
	
	def startup: Receive = {
		case jw: IndexedJob => 
			val client = sender
			log.info("Sending job {}", jw.id)
			master ! jw
			context.become(running(client, jw))
	}
	
	def running(client: ActorRef, jobWrapper: IndexedJob): Receive = {
		case Success(population: Seq[Particle[_]]) => 
			results.++=(population)
			log.info("Accumulated {} success results", results.size)
			if(results.size >= jobWrapper.job.abcParams.numParticles) { 
				log.info(s"Asking master $master to abort all")
				master ! AbortJob(jobWrapper.id)
				val finalised = results.take(jobWrapper.job.abcParams.numParticles).toSeq
				client ! finalised
				log.info("Send results seq of size {} to {}", finalised.size, client)
				context.stop(self)
			}
		case Failure(e: MaxRetryException) => 
			log.warning("Got a max retry exception", e)
	}
}