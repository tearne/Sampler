package sampler.abc.actor.root.phase.task.egen

import sampler.abc._
import sampler.abc.actor.message.{ScoredParticles, WeighedParticles}
import sampler.abc.actor.root._
import sampler.maths.Random

class EGenUtil(
    particleMixer: ParticleMixer,
    random: Random
  ) {

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

    val filtered = taggedAndScoredParamSets.seq.collect {
      case s@Scored(_, _, Some(id)) if !observedIds.contains(id) => s
    }
    val newIds = filtered.collect { case Scored(_, _, Some(id)) => id }

    gen.copy(
      dueWeighing = particlesDueWeighting.add(filtered),
      idsObserved = observedIds ++ newIds
    )
  }

  def isEnoughParticles(
      gen: EvolvingGeneration[_],
      config: ABCConfig
    ): Boolean = {

    gen.weighed.size >= config.numParticles
  }

  def emptyWeighingBuffer[P](
      gen: EvolvingGeneration[P]
    ): EvolvingGeneration[P] = {

    gen.copy(dueWeighing = ScoredParticles.empty)
  }

  def buildMixPayload[P](
      gen: EvolvingGeneration[P],
      config: ABCConfig
    ): Option[ScoredParticles[P]] = {

    particleMixer.apply(gen, config)(random)
  }
}