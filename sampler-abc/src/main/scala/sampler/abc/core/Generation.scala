package sampler.abc.core

import sampler.abc.Model
import sampler.abc.config.ABCConfig
import sampler.data.Distribution
import sampler.math.Random
import sampler.data.DistributionBuilder

sealed trait Generation[P]{
	val iteration: Int
	val tolerance: Double
	def proposalDistribution(model: Model[P], rnd: Random):Distribution[P]
}

final case class GenPrior[P](tolerance: Double) extends Generation[P]{
	val iteration = 0
	
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as massage.
	 */
	def proposalDistribution(model: Model[P], rnd: Random) = model.prior
}
final case class Population[P](
	  particleWeights: Map[P, Double],
	  iteration: Int, 
		tolerance: Double
) extends Generation[P]{
	
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as massage.
	 */
	def proposalDistribution(model: Model[P], rnd: Random) = 
		DistributionBuilder
		.fromWeightsTable(particleWeights)(rnd)
		.map(model.perturb)
}

//object Generation {
//	def init[P](model: Model[P], abcParameters: ABCConfig) = {
//		val numParticles = abcParameters.job.numParticles
//		val uniformProb = 1.0 / numParticles
//		val weightsTable = (1 to numParticles)
//			.par
//			.map(i => model.prior.sample() -> uniformProb)
//			.seq
//			.toMap
//		Generation(/*model, */0, weightsTable, Double.MaxValue)
//	}
//}