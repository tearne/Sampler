package sampler.abc.actor.main

import scala.collection.immutable.Queue
import sampler.abc.Generation
import sampler.abc.UseModelPrior
import sampler.abc.Population
import sampler.abc.Weighted
import sampler.abc.actor.sub.flushing.ToleranceCalculator
import sampler.abc.ABCConfig

case class EvolvingGeneration[P](
	currentTolerance: Double,
	previousGen: Generation[P],
	dueWeighing: ScoredParticles[P],
	weighed: WeighedParticles[P],
	idsObserved: Queue[Long]
){
	def emptyWeighingBuffer() = copy(dueWeighing = ScoredParticles.empty)
	lazy val buildingGeneration = previousGen.iteration + 1
	def mixingPool(): Seq[Weighted[P]] = 
	  weighed.seq ++ (previousGen match{
  	  case UseModelPrior(_) => Nil
  	  case pop: Population[P] => pop.weightedParticles
  	})
}