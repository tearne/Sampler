package sampler.abc

import sampler.data.Distribution

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
	def draw(): A
	final override def sample(): A = {
		val drawn = draw
		assume(density(drawn) > 0.0)
		drawn
	}
}