package sampler.maths

import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

import sampler.maths.Partition;

class PartitionTest extends FreeSpec with BeforeAndAfter {

  var seq: IndexedSeq[Double] = _
  
  before{
    seq = IndexedSeq(0.1, 0.2, 0.3, 0.4)
  }
  
  "Partition of length 4 when created" in {
    assert(Partition(seq).size === 4)
  }
  
  "Sequence of probabilities is maintained in Partition" in {
    val expected = IndexedSeq(0.1, 0.2, 0.3, 0.4)
    assert(Partition(seq).probabilities === expected)
  }
  
  "Allows partition with a zero value" in {
	val s = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
    assert(Partition(s).size === 5)
  }
  
  "Allows partition with multiple zeros" in {
	val s = IndexedSeq(0, 0.1, 0, 0.2, 0, 0.3, 0, 0.4, 0)
	assert(Partition(s).size === 9)
  }
  
  "Functionality maintained when Partition created from Weights" in {
	val s1 = IndexedSeq(0.1, 0.2, 0.3, 0.4)
	val s2 = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
	
    val p1 = Partition.fromWeights(s1)
    val p2 = Partition.fromWeights(s2)
    
    assert(p1.size === 4)
    assert(p1.probabilities === s1)
    
    assert(p2.size === 5)
  }
  
  "Error when probabilties don't sum to one" in {
    val s = IndexedSeq(0.1, 0.2, 0.2, 0.4) // sums to 0.9
    intercept[AssertionError] {
      Partition(s)
    }
  }
  
 "Cannot create Partition with zero elements" in {
    val s = IndexedSeq()
    intercept[AssertionError] {
      Partition(s)
    }
  }
  
  "Does not allow probabilities of less than zero" in {
    intercept[RangeException[Double]] {
      Partition(IndexedSeq(0.9, 0.1, -0.001, 0.001))
    }
  }
  
  "Does not allow probabilities greater than one" in {
    intercept[RangeException[Double]] {
      Partition(IndexedSeq(1.001, -0.001))
    }
  }
 
  "Does not allow weights less than zero" in {
    val s = IndexedSeq(1.0, -2, 3, 4)
    
    intercept[RangeException[Double]] {
      Partition.fromWeights(s)
    }
  }
}