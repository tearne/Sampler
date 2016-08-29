package sampler.abc

import sampler.distribution.Distribution
import sampler.maths.Random

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
	def draw(implicit r: Random): A

  def sample(implicit r: Random): A = {
		val drawn = draw(r)
		assume(density(drawn) > 0.0)
		drawn
	}
}