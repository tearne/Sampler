package sampler.cluster.abc

import sampler.cluster.actor.PortFallbackSystem
import sampler.cluster.actor.util.HostnameSetup
import sampler.io.Logging
import akka.actor.Props
import sampler.cluster.abc.slave.RootActor
import sampler.cluster.abc.population.ClusterParticleBuilder

class Node(executorFactory: => ClusterParticleBuilder) extends HostnameSetup with Logging{
	val system = PortFallbackSystem("ClusterSystem")
	val rootNodeActor = system.actorOf(Props(new RootActor(executorFactory)), name="workerroot")
	log.info("Started "+rootNodeActor)
}