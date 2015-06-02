package sampler.abc.actor.algorithm

import sampler.abc.core.ToleranceCalculator
import sampler.abc.core.WeightsHelper
import sampler.abc.core.Generation
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.WeighedParticles

class GenerationFlusher(
		toleranceCalculator: ToleranceCalculator,
		observedIdsTrimmer: ObservedIdsTrimmer,
		weightsConsolidator: WeightsHelper,
		getters: Getters,
		numParticles: Int
	){
	
	def apply[P](gen: EvolvingGeneration[P]) = {
		val weighedParticles = gen.weighed
		val currentTolerance = gen.currentTolerance
		val currentIteration = gen.currentIteration
		val model = gen.model
		val idsObserved = gen.idsObserved
		
		//Strip out tags
		val seqWeighed = getters.weighedParticlesWithoutIdTags(weighedParticles)
		
		assert(numParticles <= seqWeighed.size)
		
		val completedGen = Generation(
				model, 
				currentIteration, 
				weightsConsolidator.consolidateToWeightsTable(seqWeighed),
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