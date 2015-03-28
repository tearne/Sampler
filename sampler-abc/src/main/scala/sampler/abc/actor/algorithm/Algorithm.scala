package sampler.abc.actor.algorithm

import sampler.abc.actor.Report
import sampler.abc.actor.ScoredParticles
import sampler.abc.actor.WeighedParticles
import sampler.math.Random
import sampler.abc.config.ABCConfig
import sampler.data.Distribution

class Algorithm(
		generationFlusher: GenerationFlusher,
		particleMixer: ParticleMixer,
		getters: Getters,
		implicit val random: Random
	){
	
	def addWeightedParticles[P](
			incoming: WeighedParticles[P],
			eGen: EvolvingGeneration[P]
	): EvolvingGeneration[P] = {
		val weightedParticles = eGen.weighed
		eGen.copy(weighed = weightedParticles.add(incoming))
	}
		
	def filterAndQueueUnweighedParticles[P](
		taggedAndScoredParamSets: ScoredParticles[P],
		gen: EvolvingGeneration[P]
	): EvolvingGeneration[P] = {
		val observedIds = gen.idsObserved
		val particlesDueWeighting = gen.dueWeighing
		
		val filtered = taggedAndScoredParamSets.seq.filter(tagged => !observedIds.contains(tagged.id))
		
		gen.copy(
				dueWeighing = particlesDueWeighting.add(filtered),
				idsObserved = observedIds ++ filtered.map(_.id)
		)
	}
		
	def flushGeneration[P](gen: EvolvingGeneration[P]): EvolvingGeneration[P] = 
		generationFlusher(gen)
		
	def isEnoughParticles(gen: EvolvingGeneration[_], config: ABCConfig): Boolean =
		gen.weighed.size >= config.job.numParticles
	
	def emptyWeighingBuffer[P](gen: EvolvingGeneration[P]): EvolvingGeneration[P] = 
		gen.copy(dueWeighing = ScoredParticles.empty)
			
	//TODO can we simplify tagged and scored parm sets?
	def buildMixPayload[P](gen: EvolvingGeneration[P], abcParameters: ABCConfig): Option[ScoredParticles[P]] = {
		particleMixer.apply(gen, abcParameters)
	}
}