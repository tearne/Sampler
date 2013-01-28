package sampler.run.cluster

import akka.actor.{Actor, ActorSystem, Props}
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.kernel.Bootable
import com.sun.xml.internal.txw2.Content
import scala.util.Success
import akka.actor.ActorLogging
import akka.actor.PoisonPill
import com.typesafe.config.ConfigFactory

class TestClientBootable extends Bootable{
	val system = ActorSystem("ClusterSystem")
	
	def startup = system.actorOf(Props[TestClientActor], name = "testClient")
	def shutdown = system.shutdown()
}

object TestClientApp extends App{
	if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
	else System.setProperty("akka.remote.netty.port", "2555")
	val system = ActorSystem("ClusterSystem")
	system.actorOf(Props[TestClientActor], name = "testClient")
}

class TestClientActor extends Actor with ActorLogging{
	
	import context._
	import scala.concurrent.Await
	import akka.pattern.{ask, pipe}
//	implicit val askTimeout = akka.util.Timeout(1.minutes)
	
	val config = ConfigFactory.load.getConfig("testClient")
	val numJobs = config.getInt("numJobs")
	implicit val timeout = akka.util.Timeout(config.getInt("numJobs").minutes)
	
	val master = actorOf(Props[Master], name = "master")
	val futures = (1 to numJobs).map{i => 
		master ? Job(() => {
			(1 to 100000000).foreach{i => math.sqrt(i)}
			"---"+i.toString+"---"
		})
	}
	val f = Future.sequence(futures) mapTo manifest[IndexedSeq[String]] map(_.toString)
	f.pipeTo(self)
	
	def receive = {
		case msg => {
			log.info("*** Result ***: "+msg )
			context.system.shutdown
		}
	}
}