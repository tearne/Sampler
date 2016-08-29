package sampler.abc

import org.scalatest.FreeSpec
import sampler.maths.Random

class PriorTest extends FreeSpec {
	"Exception if samples drawn outside density support" in {
		val harness = new Prior[Int] {
			val it = (1 to 3).iterator
			
			def draw(implicit r: Random): Int = it.next;
			def density(i: Int): Double = {
				if(i == 3) 0.0
				else 1.0
			}
		}
		
		assertResult(1)(harness.sample(null))
		assertResult(2)(harness.sample(null))
		
		intercept[AssertionError]{
			harness.sample(null)
		}
	}
}