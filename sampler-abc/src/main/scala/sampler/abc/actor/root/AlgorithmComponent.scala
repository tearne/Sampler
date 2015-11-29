package sampler.abc.actor.root

import sampler.abc.actor.algorithm.Algorithm
import sampler.abc.actor.algorithm.GenerationFlusher
import sampler.abc.core.ToleranceCalculator
import sampler.abc.actor.algorithm.ObservedIdsTrimmer
import sampler.abc.core.WeightsHelper
import sampler.abc.actor.algorithm.ParticleMixer
import sampler.math.Random

trait AlgorithmCoponentImpl extends AlgorithmComponent {
	this: ABCActor[_] =>
	
	lazy val algorithm = new Algorithm(
			new GenerationFlusher(
					ToleranceCalculator,
					new ObservedIdsTrimmer(
							config.cluster.particleMemoryGenerations, 
							config.job.numParticles),
					new WeightsHelper(),
					getters,
					config.job.numParticles),
			new ParticleMixer(),
			getters,
			Random)
}

trait AlgorithmComponent {
  val algorithm: Algorithm
}