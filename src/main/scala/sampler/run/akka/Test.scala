package sampler.run.akka

import akka.util.Timeout
import scala.concurrent.Await
import scala.util.Try
import scala.concurrent.duration.DurationInt
import akka.actor.ActorSystem
import sampler.run.Job
import akka.actor.Props
import sampler.run.Aborter
import sampler.run.UserInitiatedAbortException

object Test extends App{
	System.setProperty("akka.remote.netty.port", "2553")
	val system = ActorSystem("ClusterSystem")
	//val master = system.actorOf(Props[Master], name = "worker")
	
	val jobs = (1 to 10).map{i => 
		Job((a:Aborter) => {
			//Big job
			if(i == 7) throw new RuntimeException("Induced exception")
			(1 to 10000000).foreach{j => {
				if(a.isAborted) throw new UserInitiatedAbortException("Abort flag set")
				math.sqrt(j.toDouble)
			}}
			i
		})
	}
	
	val result = new FailFastRunner(system).apply(jobs)
	
	println("Result is ..."+result)
	
}