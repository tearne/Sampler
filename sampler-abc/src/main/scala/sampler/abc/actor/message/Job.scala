package sampler.abc.actor.message

import sampler.abc.core.Generation
import sampler.abc.actor.algorithm.EvolvingGeneration
import sampler.abc.config.ABCConfig

sealed trait Job[P]
case class GenerateParticlesFrom[P](prevGen: Generation[P], config: ABCConfig) extends Job[P]

case class WeighJob[P](scored: ScoredParticles[P], prevGen: Generation[P], tolerance: Double) extends Job[P]
object WeighJob{
	def buildFrom[P](eGen: EvolvingGeneration[P]) = 
		WeighJob(
			eGen.dueWeighing,
			eGen.previousGen,
			eGen.currentTolerance
		)
}