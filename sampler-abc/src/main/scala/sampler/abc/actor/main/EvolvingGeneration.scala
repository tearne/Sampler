package sampler.abc.actor.main

import sampler.abc.Scored
import sampler.abc.Weighted
import scala.collection.immutable.Queue
import sampler.abc.Generation
import sampler.abc.UseModelPrior
import sampler.abc.Population

case class EvolvingGeneration[P](
	currentTolerance: Double,
	previousGen: Generation[P],
	dueWeighing: ScoredParticles[P],
	weighed: WeighedParticles[P],
	idsObserved: Queue[Long]
){
	def emptyWeighingBuffer = copy(dueWeighing = ScoredParticles.empty)
	lazy val buildingGeneration = previousGen.iteration + 1
}

object EvolvingGeneration {
  def buildFrom[T](completed: Generation[T]): EvolvingGeneration[T] = {
    completed match {
      case UseModelPrior(tol) =>
        EvolvingGeneration(
    			tol,
    			completed,
    			ScoredParticles.empty,
    			WeighedParticles.empty,
    			Queue.empty[Long]
    		)
      case pop: Population[T] => 
        EvolvingGeneration(
    			pop.tolerance,    //TODO should save what the next tolerance would have been in Population?
    			pop,
    			ScoredParticles.empty,
    			WeighedParticles.empty,
    			Queue.empty[Long]
    		)
    }
  }
}