package sampler.abc.actor.sub.worker

import sampler.abc.Model
import sampler.abc.Scored
import sampler.abc.Generation
import sampler.abc.UseModelPrior
import sampler.abc.Population

class ParticleWeightCalculator[P](model: Model[P], aborter: Aborter) {
	def particleWeight(particle: Scored[P], tolerance: Double, prevGen: Generation[P]): Option[Double] = {
		aborter.checkIfAborted()
		val fHat = particle.repScores.filter(_ < tolerance).size.toDouble / particle.numReps
		
		prevGen match {
			case _: UseModelPrior[P] => Some(fHat) 	
			case prevPop: Population[P] =>
				val numerator = fHat * model.prior.density(particle.params)
				val denominator = prevPop.particleWeights.map{case (prevParam, prevWeight) => 
					prevWeight * model.perturbDensity(prevParam, particle.params)
				}.sum
				/*
				 * Note, denominator should never be zero when running on one node.
				 * But when running on multiple nodes, mixing could result in 
				 * 'unsupported' particles being provided. These will be discarded
				 * during weighing. 
				 */
				if(denominator == 0) None
				else Some(numerator / denominator)
		}
	}
}