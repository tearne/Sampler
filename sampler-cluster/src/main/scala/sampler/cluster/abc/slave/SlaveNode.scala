package sampler.cluster.abc.slave

import sampler.cluster.actor.HostnameSetup
import sampler.io.Logging
import akka.actor.Props
import sampler.cluster.actor.PortFallbackSystemFactory
import sampler.abc.ABCModel
import sampler.math.Random

class SlaveNode(model: ABCModel) extends HostnameSetup with Logging{
	val system = PortFallbackSystemFactory("ABCSystem")
	
	val particleGenerator = ParticleGenerator(model, Random)
	
	val rootNodeActor = system.actorOf(Props(new RootActor(particleGenerator)), name="workerroot")
	log.info("Started "+rootNodeActor)
}