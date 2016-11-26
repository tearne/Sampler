package sampler.abc.actor.children.worker

import sampler.abc.Model
import java.util.concurrent.atomic.AtomicBoolean
import sampler.abc.actor.children.WeighJob
import scala.util.Try
import sampler.abc.actor.root.WeighedParticles
import sampler.abc.UseModelPrior
import sampler.abc.Population
import sampler.abc.Model
import sampler.abc.Generation
import sampler.abc.Weighted

trait WeigherComponentImpl[P] extends WeigherComponent[P]{
	self: AborterComponent =>
	val model: Model[P]
	lazy val weigher = new Weigher(new ParticleWeightCalculator(model, aborter))
}

trait WeigherComponent[P]{
	self: AborterComponent =>
	val weigher: Weigher[P]
	val model: Model[P]
}

class Weigher[P](calc: ParticleWeightCalculator[P]){
	def apply(job: WeighJob[P]): Try[WeighedParticles[P]] = Try{
		val result = for{
			scored <- job.scored.seq
			wt <- calc.particleWeight(scored, job.tolerance, job.prevGen) if(wt) > 0
		} yield Weighted(scored, wt)
		
		val numRejected = job.scored.size - result.size
		
		WeighedParticles(result, numRejected)
	}
}