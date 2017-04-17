package sampler.abc.actor.root.state.task.egen

import java.util.UUID

import sampler.abc.actor.message.{ScoredParticles, WeighedParticles}
import sampler.abc.{Generation, Population, UseModelPrior, Weighted}

import scala.collection.immutable.Queue

case class EvolvingGeneration[P](
  currentTolerance: Double,
  previousGen: Generation[P],
  dueWeighing: ScoredParticles[P],
  weighed: WeighedParticles[P],
  idsObserved: Queue[UUID]
) {
  def emptyWeighingBuffer() = copy(dueWeighing = ScoredParticles.empty)

  lazy val buildingGeneration = previousGen.iteration + 1

  def mixingPool(): Seq[Weighted[P]] =
    weighed.seq ++ (previousGen match {
      case UseModelPrior(_) => Nil
      case pop: Population[P] => pop.weightedParticles
    })
}