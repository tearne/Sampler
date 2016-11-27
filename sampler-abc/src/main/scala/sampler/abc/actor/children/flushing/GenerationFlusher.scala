package sampler.abc.actor.children.flushing

import sampler.abc.actor.root.{EvolvingGeneration, Getters, ScoredParticles, WeighedParticles}
import sampler.abc.{ABCConfig, Population}

import scala.collection.immutable.Queue

class GenerationFlusher(
		toleranceCalculator: ToleranceCalculator,
		observedIdsTrimmer: ObservedIdsTrimmer,
		getters: Getters,
		config: ABCConfig  //TODO send the config in messages, method args, stateData etc, not constructor?
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
			observedIdsTrimmer(idsObserved)
		)
	}

  //TODO test me
  def fromPreExistingPopulation[P](pop: Population[P]): EvolvingGeneration[P] = {
		EvolvingGeneration(
			toleranceCalculator(pop.weightedParticles, config, pop.tolerance),
			pop,
			ScoredParticles.empty,
			WeighedParticles.empty,
			Queue.empty[Long]
		)
  }

}