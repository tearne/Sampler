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
import com.typesafe.config.ConfigFactory
import sampler.io.Logging

class Dispatcher(system: ActorSystem) extends Logging{
	
	//TODO Configurable timeout
	implicit val timeout: Timeout = Try{
		val timeout = Timeout(ConfigFactory.load.getMilliseconds("sampler.futures-timeout"))
		log.info("+++++++++++++Timeout from config: "+timeout)
		timeout
	}.toOption.getOrElse{
			val default = Timeout(5.minutes)
			log.warn("Default timeout of: "+default)
			default
	}
	
	import system.dispatcher
	
	val master = system.actorOf(Props[Master], name = "master")
	val jobIDIterator = Iterator.iterate(0)(_ + 1)
	
	def apply[P](job: ABCJob[P]): Seq[Try[P]] = {
		val accActor = system.actorOf(Props(classOf[AccumulatorActor], master))
		val wrappedJob = IndexedJob(job, jobIDIterator.next)
		val result = Await.result((accActor ? wrappedJob).mapTo[Seq[Try[P]]], timeout.duration)
		accActor ! PoisonPill
		result
	}
}