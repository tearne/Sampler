package sampler.abc.actor.sub.flushing

import sampler.abc.Population
import sampler.abc.actor.main.component.helper.Getters
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.actor.main.component.helper.Getters

class GenerationFlusher(
		toleranceCalculator: ToleranceCalculator,
		observedIdsTrimmer: ObservedIdsTrimmer,
		weightsConsolidator: WeightsHelper,
		getters: Getters,
		numParticlesRequired: Int
	){
	
	def apply[P](gen: EvolvingGeneration[P]) = {
		val weighedParticles = gen.weighed
		val currentTolerance = gen.currentTolerance
		val currentGeneration = gen.buildingGeneration
		val idsObserved = gen.idsObserved
		
		//Strip out tags
		val seqWeighed = getters.weighedParticlesWithoutIdTags(weighedParticles)
		
		assert(numParticlesRequired <= seqWeighed.size)
		
		val completedGen = Population(
				weightsConsolidator.consolidateToWeightsTable(seqWeighed),
				currentGeneration, 
				currentTolerance,
				weighedParticles.acceptanceRatio
		)
			
		EvolvingGeneration(
			toleranceCalculator(seqWeighed, currentTolerance),
			completedGen,
			ScoredParticles.empty,
			WeighedParticles.empty,
			observedIdsTrimmer(idsObserved)
		)
	}
}