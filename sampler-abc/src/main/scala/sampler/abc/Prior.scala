package sampler.abc

import sampler.distribution.Distribution
import sampler.math.Random

trait Prior[A] extends Distribution[A]{
	def density(value: A): Double
	def draw(implicit r: Random): A
	
	
	//TODO would be better to seal distribution in core, but wold break this
	
	final override def sample(implicit r: Random): A = {
		val drawn = draw(r)
		assume(density(drawn) > 0.0)
		drawn
	}
}