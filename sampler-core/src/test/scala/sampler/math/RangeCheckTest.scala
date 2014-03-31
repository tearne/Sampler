package sampler.math

import org.scalatest.Matchers
import org.scalatest.FreeSpec

class RangeCheckTest extends FreeSpec with Matchers{

  "testProbabilityDoubles" in {
    RangeCheck.probability(0.0)
    RangeCheck.probability(0.2)
    RangeCheck.probability(0.55)
    RangeCheck.probability(1.0)
    
    intercept[RangeException[Double]]{
    	RangeCheck.probability(1.000001)
    }
    
    intercept[RangeException[Double]]{
    	RangeCheck.probability(-0.000001)
    }
  }
  
  "testWithinDouble" in {
	  RangeCheck.within(1.0, 2.0, 2.0)
	  RangeCheck.within(0.0, 2.0, 2.0)
	  RangeCheck.within(2.0, 2.0, 2.0)
	  RangeCheck.within(4.0, 2.0, 2.0)
	  
	  intercept[RangeException[Double]] {
		  RangeCheck.within(-1.0, 2.0, 2.0)
	  }
	  
	  intercept[RangeException[Double]] {
		  RangeCheck.within(-5.0, 2.0, 2.0)
	  }
  }
  
  "testWithinTolerance" in {
	  RangeCheck.within(5.0000001, 5.0, 1e-6)

	  intercept[RangeException[Double]] {
		  RangeCheck.within(5.000001, 5.0, 1e-6)
	  }
  }
}