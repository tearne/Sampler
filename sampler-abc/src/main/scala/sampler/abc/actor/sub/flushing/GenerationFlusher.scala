package sampler.abc.actor.sub.flushing

import sampler.abc.Population
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.ABCConfig

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