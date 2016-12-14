package sampler.abc

import org.scalatest.FreeSpec
import sampler.distribution.Distribution

class PriorTest extends FreeSpec {
	"Exception if samples drawn outside density support" in {
		val harness = new Prior[Int] {
			private val it = (1 to 3).iterator
			
			def density(i: Int): Double = {
				if(i == 3) 0.0
				else 1.0
			}

			val distribution = Distribution.from(_ => it.next)
		}
		
    println(harness.distributionSupportChecked)
		assertResult(1)(harness.distributionSupportChecked.sample(null))
		assertResult(2)(harness.distributionSupportChecked.sample(null))
		
		intercept[AssertionError]{
			harness.distributionSupportChecked.sample(null)
		}
	}
}