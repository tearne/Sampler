package sampler.abc.actor.main

import sampler.abc.Generation
import sampler.abc.Scored
import sampler.abc.Weighted

case class Start[P](initGeneration: Generation[P])
case object Failed

sealed trait WorkerResult[P]

case class ScoredParticles[P](seq: Seq[Scored[P]]) extends WorkerResult[P]{
  def size = seq.length
  def add(toAdd: ScoredParticles[P]) = ScoredParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Scored[P]]) = ScoredParticles(seq ++ toAdd)
}
object ScoredParticles{
	def empty[P] = ScoredParticles(Seq.empty[Scored[P]])
}

case class WeighedParticles[P](seq: Seq[Weighted[P]], numRejected: Int) extends WorkerResult[P] {
  def add(toAdd: WeighedParticles[P]) = WeighedParticles(seq ++ toAdd.seq, numRejected + toAdd.numRejected)
  lazy val size = seq.length
  def acceptanceRatio = if(size == 0) 0 else size.toDouble / (size + numRejected)
}
object WeighedParticles{
	def empty[P] = WeighedParticles(Seq.empty[Weighted[P]], 0)
}

case object MixNow
case class MixPayload[P](scoredParticles: ScoredParticles[P])
case object ReportCompleted
