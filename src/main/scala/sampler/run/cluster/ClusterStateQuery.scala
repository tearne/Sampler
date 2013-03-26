package sampler.run.cluster

import akka.actor.ActorSystem
import akka.actor.Props
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object ClusterStateQuery extends App{
	Try{
		java.net.InetAddress.getLocalHost.getHostAddress
	}match{
		case Success(addr) => 
			System.setProperty("akka.remote.netty.hostname", addr)
			println("Using hostname "+addr)
		case Failure(_) => println("Using config hostname")
	}
	
	apply()
	
	def apply(){
		if(args.nonEmpty) System.setProperty("akka.remote.netty.port", args(0))
		else System.setProperty("akka.remote.netty.port", "2555")
		val system = ActorSystem("ClusterSystem")
		val master = system.actorOf(Props[Master], name = "master")
		Thread.sleep(2000)
		system.shutdown
		System.exit(0)
	}
}