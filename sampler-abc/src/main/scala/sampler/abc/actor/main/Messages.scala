package sampler.abc.actor.main

import sampler.abc.core.Generation
import sampler.abc.Scored
import sampler.abc.Weighted
import sampler.abc.actor.sub.Report

case class Start[P](generationZero: Generation[P])
case object Failed
sealed trait WorkerResult[P]

case class Tagged[T](value: T, id: Long)
object Tagged{
	def apply[T](value: T): Tagged[T] = Tagged(value, System.currentTimeMillis + value.hashCode())
}

case class ScoredParticles[P](seq: Seq[Tagged[Scored[P]]]) extends WorkerResult[P]{
  def add(toAdd: ScoredParticles[P]) = ScoredParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Tagged[Scored[P]]]) = ScoredParticles(seq ++ toAdd)
  def size = seq.length
}
object ScoredParticles{
	def empty[P] = ScoredParticles(Seq.empty[Tagged[Scored[P]]])
}

final case class WeighedParticles[P](seq: Seq[Tagged[Weighted[P]]]) extends WorkerResult[P]{
  def add(toAdd: WeighedParticles[P]) = WeighedParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Tagged[Weighted[P]]]) = WeighedParticles(seq ++ toAdd)
  def size = seq.length
}
object WeighedParticles{
	def empty[P] = WeighedParticles(Seq.empty[Tagged[Weighted[P]]])
}

case object MixNow
case class MixPayload[P](scoredParticles: ScoredParticles[P])
case class ReportCompleted[P](report: Report[P])
