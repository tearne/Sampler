package sampler.math

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import sampler.Implicits._
import org.scalatest.matchers.ShouldMatchers

class StasisticComponentTest extends AssertionsForJUnit with StatisticsComponent with ShouldMatchers {

  implicit val r = Random
  val tolerance = 1e-8
  
  @Test def testRightTail {
	val empSeq = IndexedSeq[Double](1,2,3,4).toEmpirical
    
	assert(rightTail(empSeq, 1.0).value === 1.0)
	assert(rightTail(empSeq, 2.0).value === 0.75)
	assert(rightTail(empSeq, 3.0).value === 0.5)
	assert(rightTail(empSeq, 4.0).value === 0.25)
	assert(rightTail(empSeq, 5.0).value === 0)
  }
  
  @Test def calculatesEmpiricalSeqMean {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpirical
	
    mean(empSeq) should be(2.5 plusOrMinus tolerance)
  }
  
  @Test def calculatesEmpiricalTableMean {
    val empTable = Map(
		1.0 -> 1,
		2.0 -> 1,
		3.0 -> 1,
		4.0 -> 1
    ).toEmpirical
    
    mean(empTable) should be(2.5 plusOrMinus tolerance)
  }

  @Test def quantileSmallExampleEmpiricalSeq {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpirical
    
    assert(quantile(empSeq, Probability(0.25)) === 1.0)
    assert(quantile(empSeq, Probability(0.5)) === 2.0)
    assert(quantile(empSeq, Probability(0.75)) === 3.0)
  }
  
  @Test def quantileSmallExampleEmpiricalTable {
  	 val empTable = Map(
		1.0 -> 1,
		2.0 -> 2,
		3.0 -> 1,
		4.0 -> 1
    ).toEmpirical
    
    assert(quantile(empTable, Probability(0.25)) === 2.0)
    assert(quantile(empTable, Probability(0.5)) === 2.0)
    assert(quantile(empTable, Probability(0.75)) === 3.0)
  }
  
  @Test def quantileEmpiricalSeq12Primes {
    val empSeq = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27,31,37).toEmpirical
    
    assert(quantile(empSeq, Probability(0.25)) === 5.0)
    assert(quantile(empSeq, Probability(0.5)) === 13.0)
    assert(quantile(empSeq, Probability(0.75)) === 23.0)
  }

  @Test def quantileEmpiricalTable11Primes {
	val empTable = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27,31).toEmpirical
			  
	assert(quantile(empTable, Probability(0.25)) === 5.0)
	assert(quantile(empTable, Probability(0.5)) === 13.0)
	assert(quantile(empTable, Probability(0.75)) === 23.0)
  }

  @Test def quantileEmpiricalSeq10Primes {
	val empSeq = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27).toEmpirical
			  
	assert(quantile(empSeq, Probability(0.25)) === 5.0)
	assert(quantile(empSeq, Probability(0.5)) === 11.0)
	assert(quantile(empSeq, Probability(0.75)) === 19.0)
  }

  @Test def quantileEmpiricalTable9Primes {
	val empTable = IndexedSeq[Double](2,3,5,7,11,13,17,19,23).toEmpirical
			  
	assert(quantile(empTable, Probability(0.25)) === 5.0)
	assert(quantile(empTable, Probability(0.5)) === 11.0)
	assert(quantile(empTable, Probability(0.75)) === 17.0)
  }
  
  @Test def quantileAcceptSeqOfLengthOne {
    val empSeq = IndexedSeq[Double](1.0).toEmpirical
    
    assert(quantile(empSeq, Probability(0.25)) === 1.0)
	assert(quantile(empSeq, Probability(0.5)) === 1.0)
	assert(quantile(empSeq, Probability(0.75)) === 1.0)
  }
  
  @Test def quantileAssertionErrorWhenSeqOfLengthZero {
    val empSeq = IndexedSeq[Double]().toEmpirical
    
    intercept[AssertionError] {
      quantile(empSeq, Probability(0.25))
    }
  }
  
  @Test def calcualatesAbsoluteDifferenceMetric {
    val instance1 = IndexedSeq[Double](1,2,3).toEmpirical // mean 2
	val instance2 = IndexedSeq[Double](4,5,6).toEmpirical // mean 5
			
	assert(meanDistance(instance1, instance2) === 3)
  }
  
  @Test def calculatesMaximumDifferenceMetric {
    val instance1 = IndexedSeq(1,2,3,4).toEmpirical 
	val instance2 = IndexedSeq(1,2,2,2).toEmpirical // biggest distance 4
			
	maxDistance(instance1, instance2) should be(0.5 plusOrMinus tolerance)
  }
}