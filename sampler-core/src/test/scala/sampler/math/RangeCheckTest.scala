package sampler.math

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Matchers
import org.junit.Test

class RangeCheckTest extends AssertionsForJUnit with Matchers{

  @Test def testProbabilityDoubles {
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
  
  @Test def testWithinDouble {
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
  
  @Test def testWithinTolerance {
	  RangeCheck.within(5.0000001, 5.0, 1e-6)

	  intercept[RangeException[Double]] {
		  RangeCheck.within(5.000001, 5.0, 1e-6)
	  }
  }
}