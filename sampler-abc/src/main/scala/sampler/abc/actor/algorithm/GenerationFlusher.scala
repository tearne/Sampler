package sampler.abc.actor.algorithm

import sampler.abc.core.ToleranceCalculator
import sampler.abc.core.WeightsHelper
import sampler.abc.core.Generation
import sampler.abc.actor.message.ScoredParticles
import sampler.abc.actor.message.WeighedParticles
import sampler.abc.core.Population

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
		//val model = gen.model
		val idsObserved = gen.idsObserved
		
		//Strip out tags
		val seqWeighed = getters.weighedParticlesWithoutIdTags(weighedParticles)
		
		assert(numParticlesRequired <= seqWeighed.size)
		
		val completedGen = Population(
				weightsConsolidator.consolidateToWeightsTable(seqWeighed),
				currentGeneration, 
				currentTolerance
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