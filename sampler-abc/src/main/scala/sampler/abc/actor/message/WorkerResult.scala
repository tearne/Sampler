package sampler.abc.actor.message

import sampler.abc.{Scored, UUID, Weighted}

sealed trait WorkerResult[P]

case class ScoredParticles[P](seq: Seq[Scored[P]]) extends WorkerResult[P]{
  def size = seq.length
  def add(toAdd: ScoredParticles[P]) = ScoredParticles(seq ++ toAdd.seq)
  def add(toAdd: Seq[Scored[P]]) = ScoredParticles(seq ++ toAdd)
}
object ScoredParticles{
  def empty[P] = ScoredParticles(Seq.empty[Scored[P]])
}

case class WeighedParticles[P](seq: Seq[Weighted[P]], numLocalParticlesRejected: Int) extends WorkerResult[P] {
  def add(toAdd: WeighedParticles[P]) = WeighedParticles(seq ++ toAdd.seq, numLocalParticlesRejected + toAdd.numLocalParticlesRejected)
  lazy val size = seq.length

  def acceptanceRatio = {
    val numLocallyGenerated = seq.collect{case w@Weighted(Scored(_,_,Some(uuid)),_) if uuid.generatingNodeId == UUID.thisNodeId => w}.length

    if(numLocallyGenerated == 0) 0.0
    else numLocallyGenerated.toDouble / (numLocallyGenerated + numLocalParticlesRejected)
  }
}
object WeighedParticles{
  def empty[P] = WeighedParticles(Seq.empty[Weighted[P]], 0)
}
