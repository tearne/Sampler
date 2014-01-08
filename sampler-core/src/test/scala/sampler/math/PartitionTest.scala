package sampler.math

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test

class PartitionTest extends AssertionsForJUnit {

  var seq: IndexedSeq[Double] = _
  
  @Before def initialise(){
    seq = IndexedSeq(0.1, 0.2, 0.3, 0.4)
  }
  
  @Test def partitionLengthFourWhenCreated() {
    assert(Partition(seq).size === 4)
  }
  
  @Test def sequenceOfProbabilitiesMaintained() {
    val expected = IndexedSeq(0.1, 0.2, 0.3, 0.4)
    assert(Partition(seq).probabilities === expected)
  }
  
  @Test def allowsPartitionWithAValueOfZero() {
	val s = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
    assert(Partition(s).size === 5)
  }
  
  @Test def allowsPartitionWithMultipleZeroes() {
	val s = IndexedSeq(0, 0.1, 0, 0.2, 0, 0.3, 0, 0.4, 0)
	assert(Partition(s).size === 9)
  }
  
  @Test def allFunctionalityMaintainedWhenCreatedFromWeights() {
	val s1 = IndexedSeq(0.1, 0.2, 0.3, 0.4)
	val s2 = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
	
    val p1 = Partition.fromWeights(s1)
    val p2 = Partition.fromWeights(s2)
    
    assert(p1.size === 4)
    assert(p1.probabilities === s1)
    
    assert(p2.size === 5)
  }
  
  @Test def exceptionThrownWhenValuesDontSumToOne(){
    val s = IndexedSeq(0.1, 0.2, 0.2, 0.4) // sums to 0.9
    intercept[AssertionError] {
      Partition(s)
    }
  }
  
 @Test def cannotCreatePartitionWithZeroElements() {
    val s = IndexedSeq()
    intercept[AssertionError] {
      Partition(s)
    }
  }
  
  @Test def noProbsLessThanZero() {
    intercept[RangeException[Double]] {
      Partition(IndexedSeq(0.9, 0.1, -0.001, 0.001))
    }
  }
  
  @Test def noProbsGreaterThanOne() {
    intercept[RangeException[Double]] {
      Partition(IndexedSeq(1.001, -0.001))
    }
  }
 
  @Test def noWeightsLessThanZero() {
    val s = IndexedSeq(1.0, -2, 3, 4)
    
    intercept[RangeException[Double]] {
      Partition.fromWeights(s)
    }
  }
}