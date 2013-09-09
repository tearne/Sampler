package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import org.scalatest.matchers.ShouldMatchers
import sampler.math.Random
import sampler.data.Empirical._

class EmpiricalTableTest extends AssertionsForJUnit with ShouldMatchers {

  implicit val r = Random
  var tolerance: Double = 1e-6
  var d1: EmpiricalTable[Int] = _
  var d2: EmpiricalTable[Int] = _
  var d3: EmpiricalTable[Int] = _
  
  @Before def initialise {
    implicit val r = Random
    
    //	--d1--		--d2--		---d3---
	//					6		    3
	//				  5,6		  2,3
	//	4,5,6		4,5,6		1,2,3,4
    d1 = IndexedSeq(4, 5, 6).toEmpiricalTable
	d2 = IndexedSeq(4, 5,5, 6,6,6).toEmpiricalTable
	d3 = IndexedSeq(1, 2,2, 3,3,3, 4).toEmpiricalTable
  }
  
  @Test def numObservations {
  	assert(d1.size === 3)
    assert(d2.size === 6)
    assert(d3.size === 7)
  }
  
  @Test def knowsTheSupportSize {
    assert(d1.supportSize === 3)
    assert(d2.supportSize === 3)
    assert(d3.supportSize === 4)
  }
  
  @Test def calculatesRelativeProbabilitiesOfObservations {
    val e1 = 1.0/3.0
    val e2 = 1.0/6.0
    val e3 = 1.0/7.0
    
    d1.probabilityTable(4).value should be(e1 plusOrMinus tolerance)
    d2.probabilityTable(4).value should be(e2 plusOrMinus tolerance)
    d3.probabilityTable(4).value should be(e3 plusOrMinus tolerance)
  }
  
  @Test def calculatesMapOfCountsForEachObservation {
    assert(d1.freqTable === Map(4 -> 1, 5 -> 1, 6 ->1))
    assert(d2.freqTable === Map(4 -> 1, 5 -> 2, 6 ->3))
	assert(d3.freqTable === Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 1))
  }
  
  @Test def isAugmentableWithTraversableObject {
    val s1 = IndexedSeq(6)
    val d4 = d1 ++ s1
    
    assert(d4.supportSize === 3)
    d4.probabilityTable(6).value should be(0.5 plusOrMinus tolerance)
    assert(d4.freqTable === Map(4 -> 1, 5 -> 1, 6 ->2))
  }
  
  @Test def overridesHashCodeAndEquals {
    val d1a = IndexedSeq(4,5,6).toEmpiricalTable
    val empSeq = IndexedSeq(1,2,3).toEmpiricalSeq

    assert(d1.canEqual(d2))
    assertFalse(d1.canEqual(empSeq))

    assert(d1.equals(d1a))
    assert(d1.hashCode === d1a.hashCode)
    
    assertFalse(d1.equals(d2))
    assertFalse(d1.hashCode equals d2.hashCode)
  }
}
