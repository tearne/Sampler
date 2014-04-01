package sampler.data

import sampler.Implicits._
//import org.junit.Assert._
import sampler.math.Random
import sampler._
import org.scalatest.Matchers
import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

class EmpiricalSeqTest extends FreeSpec with BeforeAndAfter with Matchers {

  implicit var r: Random = _
  var es: EmpiricalSeq[Int] = _
  val tolerance: Double = 1e-6
  
  before {
	r = Random

	val seq1 = IndexedSeq(1,1,2,2,3,3,4,4)
  
	es = seq1.toEmpiricalSeq
  }
  
  "Number of observations" in {
    assert(es.size === 8)
  }
  
  "Support size" in {
    assert(es.supportSize === 4)
  }
  
  "Gives correct probabilities of observations from sequence" in {
    val probs = es.probabilityTable
    
    probs(1) should be(0.25 +- tolerance)
    probs(2) should be(0.25 +- tolerance)
    probs(3) should be(0.25 +- tolerance)
    probs(4) should be(0.25 +- tolerance)
  }
  
  "Sequence can be added to" in {
    val es2 = es.++(IndexedSeq(5,5,6,6,7,7,8,8))
    val probs = es2.probabilityTable 
    
    assert(es2.supportSize === 8)
    assert(es2.size === 16)
    probs(1) should be (0.125 +- tolerance)
    probs(5) should be (0.125 +- tolerance)
  }
  
  "Can only be added to with another empirical sequence" in {
	val es2 = es.++(IndexedSeq(5,6,7,8))
	val et = IndexedSeq(1,2,3,4).toEmpiricalTable
    
	assert(es.canEqual(es2))
	assert(!es.equals(es2))

	assert(!es.canEqual(et))
  }
  
  "Two identical sequences are identified as equal" in {
    val es2 = IndexedSeq(1,2,3,4).toEmpiricalSeq
    
    assert(es.equals(es2))
  }
}
