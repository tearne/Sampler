package sampler.cluster.abc.algorithm

import sampler.cluster.abc.Model
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted

class Weigher[P](model: Model[P]) {
	def weighAndFilter(
			scoredSeq: Seq[Scored[P]],
			previousParamsWithWeights: Map[P, Double],
			tolerance: Double
	): Seq[Weighted[P]] = {
		def doOne(particle: Scored[P]): Option[Double] = {
			val fHat = particle.repScores.filter(_ < tolerance).size.toDouble / particle.numReps
			val numerator = fHat * model.prior.density(particle.params)
			val denominator = previousParamsWithWeights.map{case (params0, weight) => 
				weight * model.perturbDensity(params0, particle.params)
			}.sum
			if(numerator > 0 && denominator > 0) Some(numerator / denominator)
			else None
		}
		
		val res = scoredSeq.map{scored => 
			val weightOption = doOne(scored)
			weightOption.map(wt => Weighted(scored, wt))
		}.flatten
		
		res
	}
}