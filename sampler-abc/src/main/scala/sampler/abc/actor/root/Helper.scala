package sampler.abc.actor.root

import sampler.abc._
import sampler.abc.actor.children.flushing.ToleranceCalculator
import sampler.maths.Random

import scala.collection.immutable.Queue

class Helper(
  particleMixer: ParticleMixer,
  val toleranceCalculator: ToleranceCalculator,
  getters: Getters,
  random: Random
) {

  //TODO test me
  def initialiseEvolvingGeneration[P](gen: Generation[P], config: ABCConfig): EvolvingGeneration[P] = {
    gen match {
      case prior: UseModelPrior[P] =>
        EvolvingGeneration(
          prior.tolerance,
          prior,
          ScoredParticles.empty,
          WeighedParticles.empty,
          Queue.empty[Long]
        )
      case pop: Population[P] =>
        EvolvingGeneration(
          toleranceCalculator(pop.weightedParticles, config, pop.tolerance),
          pop,
          ScoredParticles.empty,
          WeighedParticles.empty,
          Queue.empty[Long]
        )
    }
  }

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

  def isEnoughParticles(gen: EvolvingGeneration[_], config: ABCConfig): Boolean =
    gen.weighed.size >= config.numParticles

  def emptyWeighingBuffer[P](gen: EvolvingGeneration[P]): EvolvingGeneration[P] =
    gen.copy(dueWeighing = ScoredParticles.empty)

  def buildMixPayload[P](gen: EvolvingGeneration[P], config: ABCConfig): Option[ScoredParticles[P]] = {
    particleMixer.apply(gen, config)(random)
  }
}
