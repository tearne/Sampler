package sampler.abc.core

import sampler.abc.Model
import sampler.abc.config.ABCConfig

case class Generation[P](
	//model: Model[P],
	iteration: Int,
	particleWeights: Map[P, Double],
	tolerance: Double
)
object Generation{
	def init[P](model: Model[P], abcParameters: ABCConfig) = {
		val numParticles = abcParameters.job.numParticles
		val uniformProb = 1.0 / numParticles
		val weightsTable = (1 to numParticles)
			.par
			.map(i => model.prior.sample() -> uniformProb)
			.seq
			.toMap
		Generation(/*model, */0, weightsTable, Double.MaxValue)
	}
}