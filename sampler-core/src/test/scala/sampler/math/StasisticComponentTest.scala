package sampler.math

import sampler.Implicits._
import org.scalatest.Matchers
import org.scalatest.FreeSpec

class StasisticComponentTest extends FreeSpec with StatisticsComponent with Matchers {

  implicit val r = Random
  val tolerance = 1e-8
  val statistics = new Statistics{}
  import statistics._
  
  "Testing right tail values" in {
  	
	val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
    
	assert(rightTail(empSeq, 1.0) === 1.0)
	assert(rightTail(empSeq, 2.0) === 0.75)
	assert(rightTail(empSeq, 3.0) === 0.5)
	assert(rightTail(empSeq, 4.0) === 0.25)
	assert(rightTail(empSeq, 5.0) === 0)
  }
  
  "Calculates empirical sequence mean" in {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
	
    mean(empSeq) should be(2.5 +- tolerance)
  }
  
  "Calculates empirical table mean" in {
    val empTable = IndexedSeq[Double](1,2,3,4).toEmpiricalTable
    
    mean(empTable) should be(2.5 +- tolerance)
  }

  "Quantiles from an empirical sequence" in {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
    
    val quantiles = quantile(empSeq, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(1.0,2.0,3.0))
  }
  
  "Quantiles from an empirical table" in {
    val empTable = IndexedSeq[Double](1,2,2,3,4).toEmpiricalTable
    
    val quantiles = quantile(empTable, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(2.0,2.0,3.0))
  }
  
  "Quantiles from an empirical sequence of 12 primes" in {
    val empSeq = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27,31,37).toEmpiricalSeq

    val quantiles = quantile(empSeq, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(5.0,13.0,23.0))
  }

  "Quantiles from an empirical table of 11 primes" in {
	val empTable = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27,31).toEmpiricalSeq
	
	val quantiles = quantile(empTable, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(5.0,13.0,23.0))
  }

  "Quantiles from an empirical sequence of 10 primes" in {
	val empSeq = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27).toEmpiricalSeq
	
	val quantiles = quantile(empSeq, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(5.0,11.0,19.0))
  }

  "Quantiles from an empirical talbe of 9 primes" in {
	val empTable = IndexedSeq[Double](2,3,5,7,11,13,17,19,23).toEmpiricalSeq
	
	val quantiles = quantile(empTable, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(5.0,11.0,17.0))
  }
  
  "Quantile accepts sequence of length 1" in {
    val empSeq = IndexedSeq[Double](1.0).toEmpiricalSeq
    
    val quantiles = quantile(empSeq, Seq(0.25, 0.5, 0.75))
    assert(quantiles === Seq(1.0,1.0,1.0))
  }
  
  "Error when sequence of lenght zero to quantile" in {
    val empSeq = IndexedSeq[Double]().toEmpiricalSeq
    
    intercept[AssertionError] {
      quantile(empSeq, Seq(0.25))
    }
  }
  
  "Exception when negative probability supplied" in {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
    
    intercept[RangeException[Double]] {
      quantile(empSeq, Seq(0.25, -0.5))
    }
  }  

  "Exception when probability greater than one supplied" in {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
    
    intercept[RangeException[Double]] {
      quantile(empSeq, Seq(0.25, 5.0))
    }
  }
  
  "calcualatesAbsoluteDifferenceMetric" in {
    val instance1 = IndexedSeq[Double](1,2,3).toEmpiricalSeq // mean 2
	val instance2 = IndexedSeq[Double](4,5,6).toEmpiricalSeq // mean 5
			
	assert(meanDistance(instance1, instance2) === 3)
  }
  
  "calculatesMaximumDifferenceMetric" in {
    val instance1 = IndexedSeq(1,2,3,4).toEmpiricalSeq 
	val instance2 = IndexedSeq(1,2,2,2).toEmpiricalSeq // biggest distance 4
			
	maxDistance(instance1, instance2) should be(0.5 +- tolerance)
  }
}