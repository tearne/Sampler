package sampler.abc.actor.children.worker

import sampler.abc.Model
import sampler.abc.Generation
import sampler.abc.UseModelPrior
import sampler.abc.Population
import sampler.abc.{Weighted, Scored}

class ParticleWeightCalculator[P](model: Model[P], aborter: Aborter) {
	def particleWeight(particle: Scored[P], tolerance: Double, prevGen: Generation[P]): Option[Double] = {
		aborter.checkIfAborted()
		val fHat = particle.scores.count(_ < tolerance).toDouble / particle.scores.size
		
		prevGen match {
			case _: UseModelPrior[P] => Some(fHat) 	
			case prevPop: Population[P] =>
				val numerator = fHat * model.prior.density(particle.params)
				val denominator = prevPop.weightedParticles.map{case Weighted(Scored(prevParam, _, _), prevWeight) => 
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