package sampler.abc

import sampler.abc.config.ABCConfig
import sampler.data.DistributionBuilder
import sampler.math.Random
import sampler.abc.actor.sub.Report

class Reporter(
		builder: DistributionBuilder, 
		random: Random,
		config: ABCConfig
) {
	def build[P](gen: Generation[P]): Report[P] = {
	  /*
	   * 
	   * 
	   * DANGER!
	   * 
	   * 
	   */
	  //TODO we are just stripping out the weights, not sampling from them or printing them.  This needs fixing urgently
	  
	  
		val numParticles = config.job.numParticles
		
		val unweightedParameters = gen match {
			case _: UseModelPrior[P] => throw new UnsupportedOperationException //TODO  better
			case pop: Population[P] => 
				pop.particleWeights.keys
		}
		
//		val samples: Seq[P] = builder
//				.fromWeightsTable(weights)(random)
//				.until(_.size == numParticles)
//				.sample
				
		//TODO consider changing the report to output a weights table instead of sampling?
		Report(
				gen.iteration,
				gen.tolerance,
				unweightedParameters.toSeq
		)
	}
}