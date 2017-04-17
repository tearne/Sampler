package sampler.abc.actor.children.flushing

import sampler.abc.actor.message.{ScoredParticles, WeighedParticles}
import sampler.abc.actor.root.state.task.egen.EvolvingGeneration
import sampler.abc.{ABCConfig, Population}

import scala.collection.immutable.Queue

class GenerationFlusher(
		toleranceCalculator: ToleranceCalculator,
		config: ABCConfig
	){
	
	def fromEvolvingGen[P](gen: EvolvingGeneration[P]) = {
		val weighedParticles = gen.weighed
		val currentTolerance = gen.currentTolerance
		val currentGeneration = gen.buildingGeneration
		val idsObserved = gen.idsObserved
		
		assert(config.numParticles <= weighedParticles.size)
		
		val completedGen = Population(
				weighedParticles.seq,
				currentGeneration,
				currentTolerance,
				weighedParticles.acceptanceRatio
		)

		EvolvingGeneration(
			toleranceCalculator(weighedParticles.seq, config, currentTolerance),
			completedGen,
			ScoredParticles.empty,
			WeighedParticles.empty,
			idsObserved
		)
	}

  //TODO test me
  def fromPreExistingPopulation[P](pop: Population[P]): EvolvingGeneration[P] = {
		EvolvingGeneration(
			toleranceCalculator(pop.weightedParticles, config, pop.tolerance),
			pop,
			ScoredParticles.empty,
			WeighedParticles.empty,
			Queue.empty
		)
  }

}