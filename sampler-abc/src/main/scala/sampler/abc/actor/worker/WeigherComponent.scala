package sampler.abc.actor.worker

import java.util.concurrent.atomic.AtomicBoolean
import scala.Option.option2Iterable
import scala.util.Try
import sampler.abc.Model
import sampler.abc.Scored
import sampler.abc.Weighted
import sampler.abc.actor.Tagged
import sampler.abc.actor.WeighedParticles
import sampler.abc.Weighted
import sampler.abc.actor.WeighJob

trait WeigherComponent[P] {
	val weigher: Weigher
	val model: Model[P]
	
	trait Weigher {
		val aborted: AtomicBoolean = new AtomicBoolean(false)
		def abort() { aborted.set(true) }
		def reset() { aborted.set(false) }
		private def isAborted = aborted.get
		
		def run(job: WeighJob[P]): Try[WeighedParticles[P]] = Try{
//			import job._
				
			def getParticleWeight(particle: Scored[P]): Option[Double] = {
				
				if(isAborted) throw new DetectedAbortionException("Abort flag was set")
				
				val fHat = particle.repScores.filter(_ < job.tolerance).size.toDouble / particle.numReps
				val numerator = fHat * model.prior.density(particle.params)
				val denominator = job.previousPopulation.map{case (params0, weight) => 
					weight * model.perturbDensity(params0, particle.params)
				}.sum
				if(numerator > 0 && denominator > 0) Some(numerator / denominator)
				else None
			}
			
			val result = for{
				p <- job.scored.seq
				wt <- getParticleWeight(p.value)
			} yield Tagged(Weighted(p.value, wt), p.id)
			
			WeighedParticles(result)
		}
	}
}