package sampler.run.cluster

import akka.actor.ActorSystem
import akka.actor.Props

object ClusterStateQuery extends App{
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