package sampler.abc.actor.children.flushing

import sampler.abc.{ABCConfig, Population}
import sampler.abc.actor.root.{EvolvingGeneration, Getters, ScoredParticles, WeighedParticles}

class GenerationFlusher(
		toleranceCalculator: ToleranceCalculator,
		observedIdsTrimmer: ObservedIdsTrimmer,
		getters: Getters,
		config: ABCConfig
	){
	
	def apply[P](gen: EvolvingGeneration[P]) = {
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
}