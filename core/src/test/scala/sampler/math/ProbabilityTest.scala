package sampler.math

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
//import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ProbabilityTest extends AssertionsForJUnit with ShouldMatchers{

  var p1: Probability = _
  var p2: Probability = _
  
  @Before def initialise(){
    p1 = Probability(0.3)
    p2 = Probability(0.2)
  }
  
  @Test def errorIfValueGreaterThanOne {
    intercept[AssertionError] {
      val probability = Probability(1.01)
    }
  }
  
  @Test def errorIfValueLessThanZero{
    intercept[AssertionError] {
      val probability = Probability(-0.5)
    }
  }
  
  @Test def valueBasedHashCodeAndEquals {
    val p1a = Probability(0.3)
    
    assert(p1 === p1a)
    assert(p1.hashCode === p1a.hashCode)
    
    assert(p1 != p2)
    assert(p1.hashCode != p2.hashCode)
  }
  
  @Test def probabilitiesHaveRegularMathematicalOperations {
	val p3 = Probability(0.4)
	val tolerance = 1e-8
    
    (p1 + p2).value should be (0.5 plusOrMinus tolerance)
	(p1 - p2).value should be (0.1 plusOrMinus tolerance)
	(p1 * p2).value should be (0.06 plusOrMinus tolerance)
	(p2 / p3).value should be (0.5 plusOrMinus tolerance)
  }
  
  @Test def implicitToDoubleValue {
    assert(p1.toDouble === 0.3)
  }
  
  @Test def implicitImplementationOfFractionalGivesCorrectResults {
    val frac = implicitly[Fractional[Probability]]
    
    val tolerance = 1e-8
    
    assert(frac.compare(p1, p2) > 0)
    assert(frac.compare(p2, p1) < 0)
    assert(frac.compare(p2, p2) === 0)
    
    frac.plus(p1, p2).value should be(0.5 plusOrMinus tolerance)
    frac.minus(p1, p2).value should be(0.1 plusOrMinus tolerance)
    
    frac.times(p1, p2).value should be(0.06 plusOrMinus tolerance)
    frac.div(p2, Probability(0.4)).value should be(0.5 plusOrMinus tolerance)
    
    assert(frac.negate(Probability(0.0)) === Probability(0.0))
    intercept[AssertionError] {
      frac.negate(p1)
    }
    
    assert(frac.fromInt(1) === Probability(1))
    intercept[AssertionError] {
      frac.fromInt(2)
    }
    
    assert(frac.toInt(p1) === 0)
    assert(frac.toInt(Probability(0.99)) === 0)
    
    assert(frac.toLong(p1) === 0)
    assert(frac.toLong(Probability(0.99)) === 0)
    
    assert(frac.toFloat(p1) === 0.3.toFloat)
    assert(frac.toDouble(p1) === 0.3)
  }
}