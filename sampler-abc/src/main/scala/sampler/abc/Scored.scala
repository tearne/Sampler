package sampler.abc

case class Scored[P](params: P, scores: Seq[Double], id: Option[Long]){
  def meanScore: Double = scores.sum.toDouble / scores.size
}
object Scored{
  def apply[P](params: P, scores: Seq[Double]): Scored[P] =
    Scored(
      params, 
      scores,
      Some(System.currentTimeMillis + 7 * params.hashCode())
    )
}