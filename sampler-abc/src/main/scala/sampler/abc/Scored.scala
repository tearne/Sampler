package sampler.abc

case class Scored[P](params: P, scores: Seq[Double], id: Option[UUID]){
  def meanScore: Double = scores.sum / scores.size
  def wasLocallyGenerated: Boolean = id.exists(_.generatingNodeId == UUID.thisNodeId)
}

object Scored{
  def apply[P](params: P, scores: Seq[Double]): Scored[P] =
    Scored(
      params, 
      scores,
      Some(UUID.generate())
    )
}