package sampler.abc.actor.main.component

import sampler.abc.actor.sub.flushing.GenerationFlusher
import sampler.abc.actor.sub.flushing.ToleranceCalculator
import sampler.abc.actor.sub.flushing.ObservedIdsTrimmer
import sampler.abc.actor.sub.flushing.WeightsHelper
import sampler.math.Random
import sampler.abc.actor.main.MainActor
import sampler.abc.actor.main.component.helper.ParticleMixer
import sampler.abc.config.ABCConfig
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.actor.main.component.helper.Getters

trait HelperCoponentImpl extends HelperComponent {
	this: MainActor[_] =>
	
	lazy val helper = new Helper(
			new ParticleMixer(),
			getters,
			Random)
}

trait HelperComponent {
  val helper: Helper
}

class Helper(
		particleMixer: ParticleMixer,
		getters: Getters,
		random: Random
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
		
	def isEnoughParticles(gen: EvolvingGeneration[_], config: ABCConfig): Boolean =
		gen.weighed.size >= config.job.numParticles
	
	def emptyWeighingBuffer[P](gen: EvolvingGeneration[P]): EvolvingGeneration[P] = 
		gen.copy(dueWeighing = ScoredParticles.empty)
			
	//TODO can we simplify tagged and scored parm sets?
	def buildMixPayload[P](gen: EvolvingGeneration[P], abcParameters: ABCConfig): Option[ScoredParticles[P]] = {
		particleMixer.apply(gen, abcParameters)(random)
	}
}