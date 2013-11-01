package sampler.cluster.abc.slave

import sampler.cluster.actor.util.HostnameSetup
import sampler.io.Logging
import sampler.cluster.actor.PortFallbackSystem
import akka.actor.Props
import com.typesafe.config.ConfigFactory
import akka.actor.Actor
import akka.actor.ActorLogging
import sampler.cluster.abc.population.ClusterParticleBuilder

class RootActor(builderFactory: => ClusterParticleBuilder) extends Actor with ActorLogging{
	val n = ConfigFactory.load().getInt("sampler.node.workers-per")
	val numWorkers = if(n <= 0) Runtime.getRuntime().availableProcessors() else n

	(1 to numWorkers).foreach(i => context.actorOf(Props(new Worker(builderFactory))))
	
	log.info(s"Started $numWorkers workers")
	
	def receive = {
		case StatusRequest => 
			context.children.foreach{child => 
				child.forward(StatusRequest)
			}
			log.debug("Forwarded {} to {}", StatusRequest, context.children.size)
		case m => log.error("Unexpected message {} from {}",m, sender)
	}
}