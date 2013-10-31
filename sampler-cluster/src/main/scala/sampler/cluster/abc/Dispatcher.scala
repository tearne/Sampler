package sampler.cluster.abc

import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.util.Timeout
import scala.util.Try
import akka.actor.Props
import akka.pattern.ask
import scala.concurrent.Await
import sampler.cluster.abc.master.AccumulatorActor
import akka.actor.PoisonPill
import sampler.cluster.abc.master.Master
import sampler.cluster.abc.slave.IndexedJob

class Dispatcher(system: ActorSystem) {
	//TODO Configurable timeout
	implicit val timeout = Timeout(5.minutes)
	import system.dispatcher
	
	val master = system.actorOf(Props[Master], name = "master")
	val jobIDIterator = Iterator.iterate(0)(_ + 1)
	
	def apply[P](job: ABCJob[P]): Seq[Try[P]] = {
		val ff = system.actorOf(Props(classOf[AccumulatorActor], master))
		val wrappedJob = IndexedJob(job, jobIDIterator.next)
		val result = Await.result((ff ? wrappedJob).mapTo[Seq[Try[P]]], timeout.duration)
		ff ! PoisonPill
		result
	}
}