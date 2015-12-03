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
		
		val weight: Option[Double] = prevGen match {
			case _: UseModelPrior[P] => Some(fHat) 	
			case prevPop: Population[P] =>
				val numerator = fHat * model.prior.density(particle.params)
				val denominator = prevPop.particleWeights.map{case (prevParam, prevWeight) => 
				  //assume(prevParam != particle.params, println ("Previous particle could have perturbed to this one"))
					prevWeight * model.perturbDensity(prevParam, particle.params)
				}.sum
				if(numerator > 0 && denominator > 0) Some(numerator / denominator)
				else None
		}
		weight
	}
}