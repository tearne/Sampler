package sampler.abc.actor.main

case class Scored[P](params: P, scores: Seq[Double], id: Long){
  def meanScore: Double = scores.sum.toDouble / scores.size
}
object Scored{
  def apply[P](params: P, scores: Seq[Double]): Scored[P] =
    Scored(
      params, 
      scores,
      System.currentTimeMillis + 7 * params.hashCode()
    )
}

case class Weighted[P](scored: Scored[P], weight: Double){
  def meanScore: Double = scored.meanScore
}