package sampler.math

import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

class PartitionTest extends FreeSpec with BeforeAndAfter {

  var seq: IndexedSeq[Double] = _
  
  before{
    seq = IndexedSeq(0.1, 0.2, 0.3, 0.4)
  }
  
  "partitionLengthFourWhenCreated" in {
    assert(Partition(seq).size === 4)
  }
  
  "sequenceOfProbabilitiesMaintained" in {
    val expected = IndexedSeq(0.1, 0.2, 0.3, 0.4)
    assert(Partition(seq).probabilities === expected)
  }
  
  "allowsPartitionWithAValueOfZero()" in {
	val s = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
    assert(Partition(s).size === 5)
  }
  
  "allowsPartitionWithMultipleZeroes" in {
	val s = IndexedSeq(0, 0.1, 0, 0.2, 0, 0.3, 0, 0.4, 0)
	assert(Partition(s).size === 9)
  }
  
  "allFunctionalityMaintainedWhenCreatedFromWeights" in {
	val s1 = IndexedSeq(0.1, 0.2, 0.3, 0.4)
	val s2 = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
	
    val p1 = Partition.fromWeights(s1)
    val p2 = Partition.fromWeights(s2)
    
    assert(p1.size === 4)
    assert(p1.probabilities === s1)
    
    assert(p2.size === 5)
  }
  
  "exceptionThrownWhenValuesDontSumToOne" in {
    val s = IndexedSeq(0.1, 0.2, 0.2, 0.4) // sums to 0.9
    intercept[AssertionError] {
      Partition(s)
    }
  }
  
 "cannotCreatePartitionWithZeroElements" in {
    val s = IndexedSeq()
    intercept[AssertionError] {
      Partition(s)
    }
  }
  
  "noProbsLessThanZero" in {
    intercept[RangeException[Double]] {
      Partition(IndexedSeq(0.9, 0.1, -0.001, 0.001))
    }
  }
  
  "noProbsGreaterThanOne" in {
    intercept[RangeException[Double]] {
      Partition(IndexedSeq(1.001, -0.001))
    }
  }
 
  "noWeightsLessThanZero" in {
    val s = IndexedSeq(1.0, -2, 3, 4)
    
    intercept[RangeException[Double]] {
      Partition.fromWeights(s)
    }
  }
}