package sampler.abc.actor.root

import sampler.abc.config.ABCConfig
import sampler.abc.actor.WeighedParticles
import sampler.abc.algorithm.Generation

trait GettersComponent{
	val getters: Getters
}

trait Getters {
	def getMixRateMS(config: ABCConfig): Long = config.cluster.mixRateMS
	def getNumParticles(config: ABCConfig): Int = config.job.numParticles
	
	//TODO so far these are just used in logging, to avoid errors when testing, but they could
	// be used more widely, or perhaps converted to lenses?
	def getNumParticles[P](weighedParticles: WeighedParticles[P]): Int = weighedParticles.seq.size
	def getAccumulatedGenerationSize[P](generation: Generation[P]): Int = generation.weighted.size
}