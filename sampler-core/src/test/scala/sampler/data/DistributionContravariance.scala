package sampler.data

import sampler.math.Random

class DistributionContravariance {
  
  /*
   * Covariance and contravariance tests (for compilation only)
   * If this code compiles it is working 
   */
  object PlayingWithVariance{
  	class T
		class S extends T
		
		val isT: T = new S
				
		val t = new Distribution[T]{
		  def sample() = new T
		}
		
		val s = new Distribution[S]{
		  def sample() = new S
		}
				
		val res1 = t.sample()
		val res2 = s.sample()
		val res3 = s.sample()
				
		val u: Distribution[T] = s
		val w: Distribution[S] = s
				
		class U
		val p: Distribution[U] = t.combine(s)((a:T, b:S) => new U)
  }
}