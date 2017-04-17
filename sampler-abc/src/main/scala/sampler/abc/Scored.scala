package sampler.abc

import java.util.UUID

case class Scored[P](params: P, scores: Seq[Double], id: Option[UUID]){
  def meanScore: Double = scores.sum / scores.size
}
object Scored{
  def apply[P](params: P, scores: Seq[Double]): Scored[P] =
    Scored(
      params, 
      scores,
      Some(UUID.randomUUID)
    )
}