package sampler.math

class RangeException[N: Numeric](actual: N, lower: N, upper: N) 
	extends RuntimeException(s"$actual not in range [$lower, $upper]")

object RangeCheck {
	def probability[T: Fractional](p: T) {
		val f = implicitly[Fractional[T]]
		if(f.lt(p, f.zero) || f.gt(p, f.one)) 
			throw new RangeException(p, f.zero, f.one)
	}
	def within[T: Fractional](actual: T, expected: T, tolerance: T) {
		val f = implicitly[Fractional[T]]
		import f.mkNumericOps
		if(f.gt(f.abs(actual - expected), tolerance)) 
			throw new RangeException(actual, expected - tolerance, actual + tolerance)
	}
}