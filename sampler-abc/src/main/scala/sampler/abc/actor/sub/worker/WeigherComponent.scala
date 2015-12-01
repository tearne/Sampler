package sampler.abc.actor.sub.worker

import sampler.abc.Model
import java.util.concurrent.atomic.AtomicBoolean
import sampler.abc.actor.sub.WeighJob
import scala.util.Try
import sampler.abc.Scored
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.UseModelPrior
import sampler.abc.Population
import sampler.abc.Model
import sampler.abc.Weighted
import sampler.abc.actor.main.Tagged
import sampler.abc.Generation

trait WeigherComponentImpl[P] extends WeigherComponent[P]{
	self: AborterComponent =>
	val model: Model[P]
	val weigher = new Weigher(new ParticleWeightCalculator(model, aborter))
}

trait WeigherComponent[P]{
	self: AborterComponent =>
	val weigher: Weigher[P]
	val model: Model[P]
}

class Weigher[P](calc: ParticleWeightCalculator[P]){
	def apply(job: WeighJob[P]): Try[WeighedParticles[P]] = Try{
		val result = for{
			p <- job.scored.seq
			wt <- calc.particleWeight(p.value, job.tolerance, job.prevGen)
		} yield Tagged(Weighted(p.value, wt), p.id)
		
		WeighedParticles(result)
	}
}

class ParticleWeightCalculator[P](model: Model[P], aborter: Aborter) {
	def particleWeight(particle: Scored[P], tolerance: Double, prevGen: Generation[P]): Option[Double] = {
		aborter.checkIfAborted()
		val fHat = particle.repScores.filter(_ < tolerance).size.toDouble / particle.numReps
		
		val weight: Option[Double] = prevGen match {
			case _: UseModelPrior[P] => if(fHat >0) Some(fHat) else None	
			case prevPop: Population[P] =>
				val numerator = fHat * model.prior.density(particle.params)
				val denominator = prevPop.particleWeights.map{case (prevParam, prevWeight) => 
					prevWeight * model.perturbDensity(prevParam, particle.params)
				}.sum
				if(numerator > 0 && denominator > 0) Some(numerator / denominator)
				else None
		}
		weight
	}
}