package sampler.abc

import sampler.data.Distribution
import sampler.math.Random
import sampler.data.DistributionBuilder

sealed trait Generation[P]{
	val iteration: Int
	val tolerance: Double
	def proposalDistribution(model: Model[P], rnd: Random):Distribution[P]
}

case class UseModelPrior[P](tolerance: Double = Double.MaxValue) extends Generation[P]{
	val iteration = 0
	
	/*
	 *  Model & Random are args rather than in constructor so 
	 *  this class can safely be serialised and used as massage.
	 */
	def proposalDistribution(model: Model[P], rnd: Random) = model.prior
}
case class Population[P](
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