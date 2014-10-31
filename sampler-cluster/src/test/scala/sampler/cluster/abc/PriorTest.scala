package sampler.cluster.abc

import org.scalatest.FreeSpec
import sampler.data.Distribution

class PriorTest extends FreeSpec {
	"Exception if samples drawn outside density support" in {
		val harness = new Prior[Int] {
			val it = (1 to 3).iterator
			
			def draw(): Int = it.next;
			def density(i: Int): Double = {
				if(i == 3) 0.0
				else 1.0
			}
		}
		
		assertResult(1)(harness.sample())
		assertResult(2)(harness.sample())
		
		intercept[AssertionError]{
			harness.sample()
		}
	}
}