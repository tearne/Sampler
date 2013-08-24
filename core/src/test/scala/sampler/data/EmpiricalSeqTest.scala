package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import sampler.math.Random
import sampler.data.Empirical._

class EmpiricalSeqTest extends AssertionsForJUnit with ShouldMatchers {

  implicit var r: Random = _
  var es: EmpiricalSeq[Int] = _
  val tolerance: Double = 1e-6
  
  @Before def initialise {
	r = Random

	val seq1 = IndexedSeq(1,1,2,2,3,3,4,4)
  
	es = seq1.toEmpiricalSeq
  }
  
  @Test def numObservations {
    assert(es.size === 8)
  }
  
  @Test def supportSize {
    assert(es.supportSize === 4)
  }
  
  @Test def empiricalSeqGivesCorrectProbabilities {
    val probs = es.probabilityTable
    
    probs(1).value should be(0.25 plusOrMinus tolerance)
    probs(2).value should be(0.25 plusOrMinus tolerance)
    probs(3).value should be(0.25 plusOrMinus tolerance)
    probs(4).value should be(0.25 plusOrMinus tolerance)
  }
  
  @Test def empiricalSeqCanBeAddedTo {
    val es2 = es.++(IndexedSeq(5,5,6,6,7,7,8,8))
    val probs = es2.probabilityTable 
    
    assert(es2.supportSize === 8)
    assert(es2.size === 16)
    probs(1).value should be (0.125 plusOrMinus tolerance)
    probs(5).value should be (0.125 plusOrMinus tolerance)
  }
  
  @Test def empiricalSeqCanEqual {
	val es2 = es.++(IndexedSeq(5,6,7,8))
    
	assert(es.canEqual(es2))
	assertFalse(es.equals(es2))
  }
  
  @Test def empiricalSeqIsEqual {
    val es2 = IndexedSeq(1,2,3,4).toEmpiricalSeq
    
    assert(es.equals(es2))
  }
}