package sampler.cluster.abc.actor.root

import sampler.cluster.abc.actor.Lenses
import sampler.cluster.abc.config.ABCConfig

trait GettersComponent{
	val getters: Getters = new Getters{}
}

trait Getters {
	def getMixRateMS(config: ABCConfig): Long = config.cluster.mixRateMS
	def getNumParticles(config: ABCConfig): Int = config.job.numParticles
}