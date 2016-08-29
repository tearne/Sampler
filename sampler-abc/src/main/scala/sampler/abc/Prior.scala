package sampler.abc

import sampler.distribution.Distribution
import sampler._

trait Prior[A] {
  def density(value: A): Double

  protected val distribution: Distribution[A]

  def distributionSupportChecked: Distribution[A] =
    distribution.map { drawn => assume(density(drawn) > 0.0); drawn }
}