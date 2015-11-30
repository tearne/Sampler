package sampler.abc.actor.algorithm

import akka.actor.ActorRef
import sampler.abc.actor.Tagged
import sampler.abc.Model
import sampler.abc.config.ABCConfig
import sampler.abc.actor.message.ScoredParticles
import sampler.abc.actor.message.WeighedParticles
import sampler.abc.Scored
import sampler.abc.Weighted
import scala.collection.immutable.Queue
import sampler.abc.core.Generation

case class EvolvingGeneration[P](
	currentTolerance: Double,
	previousGen: Generation[P],
	dueWeighing: ScoredParticles[P],
	weighed: WeighedParticles[P],
	idsObserved: Queue[Long]
){
	def emptyWeighingBuffer = copy(dueWeighing = ScoredParticles.empty)
	val buildingGeneration = previousGen.iteration + 1
	//val model = previousGen.model
}

object EvolvingGeneration {
	def init[P](completed: Generation[P]): EvolvingGeneration[P] = {
		EvolvingGeneration(
			Double.MaxValue,
			completed,
			ScoredParticles(Seq.empty[Tagged[Scored[P]]]),
			WeighedParticles(Seq.empty[Tagged[Weighted[P]]]),
			Queue.empty[Long]
		)
	}
}