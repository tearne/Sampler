package sampler.abc

case class Weighted[P](scored: Scored[P], weight: Double){
  def meanScore: Double = scored.meanScore
  def wasLocallyGenerated: Boolean = scored.wasLocallyGenerated
}