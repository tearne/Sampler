package sampler.abc.core

import sampler.abc.config.ABCConfig
import sampler.abc.actor.Report
import sampler.data.DistributionBuilder
import sampler.math.Random

class Reporter(
		builder: DistributionBuilder, 
		random: Random,
		config: ABCConfig
) {
	def build[P](gen: Generation[P]): Report[P] = {
		val numParticles = config.job.numParticles
		val samples: Seq[P] = builder
				.fromWeightsTable(gen.particleWeights)(random)
				.until(_.size == numParticles)
				.sample

				
		//TODO consider changing the report to output a weights table instead of sampling?
		Report(
				gen.iteration,
				gen.tolerance,
				samples
		)
	}
}