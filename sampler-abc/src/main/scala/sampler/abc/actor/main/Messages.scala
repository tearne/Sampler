package sampler.abc.actor.main

import sampler.abc.Generation
import sampler.abc.Scored
import sampler.abc.Weighted

case class Start[P](initGeneration: Generation[P])
case object Failed

case class Tagged[T](value: T, id: Long)
object Tagged{
	def apply[T](value: T): Tagged[T] = Tagged(value, System.currentTimeMillis + value.hashCode())
}

sealed trait WorkerResult[P]

case class ScoredParticles[P](seq: Seq[Tagged[Scored[P]]]) extends WorkerResult[P]{
  def add(toAdd: ScoredParticles[P]) = ScoredParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Tagged[Scored[P]]]) = ScoredParticles(seq ++ toAdd)
  def size = seq.length
}
object ScoredParticles{
	def empty[P] = ScoredParticles(Seq.empty[Tagged[Scored[P]]])
}

case class WeighedParticles[P](seq: Seq[Tagged[Weighted[P]]], numRejected: Int) extends WorkerResult[P]{
  def add(toAdd: WeighedParticles[P]) = WeighedParticles(seq ++ toAdd.seq, numRejected + toAdd.numRejected)
//  def add(toAdd: Seq[Tagged[Weighted[P]]], numRejected: Int) = WeighedParticles(seq ++ toAdd, numRejected)
  lazy val size = seq.length
  def acceptanceRatio = if(size == 0) 0 else size.toDouble / (size + numRejected)
}
object WeighedParticles{
	def empty[P] = WeighedParticles(Seq.empty[Tagged[Weighted[P]]], 0)
}

case object MixNow
case class MixPayload[P](scoredParticles: ScoredParticles[P])
case object ReportCompleted
