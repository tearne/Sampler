package sampler.data

import sampler.math.Random
import sampler.Implicits._
import org.scalatest.Matchers
import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

class EmpiricalTableTest extends FreeSpec with BeforeAndAfter with Matchers {

  implicit val r = Random
  var tolerance: Double = 1e-6
  var d1: EmpiricalTable[Int] = _
  var d2: EmpiricalTable[Int] = _
  var d3: EmpiricalTable[Int] = _
  
  before {
    implicit val r = Random
    
    //	--d1--		--d2--		---d3---
	//					6		    3
	//				  5,6		  2,3
	//	4,5,6		4,5,6		1,2,3,4
    d1 = IndexedSeq(4, 5, 6).toEmpiricalTable
	d2 = IndexedSeq(4, 5,5, 6,6,6).toEmpiricalTable
	d3 = IndexedSeq(1, 2,2, 3,3,3, 4).toEmpiricalTable
  }
  
  "numObservations" in  {
  	assert(d1.size === 3)
    assert(d2.size === 6)
    assert(d3.size === 7)
  }
  
  "knowsTheSupportSize" in {
    assert(d1.supportSize === 3)
    assert(d2.supportSize === 3)
    assert(d3.supportSize === 4)
  }
  
  "calculatesRelativeProbabilitiesOfObservations" in {
    val e1 = 1.0/3.0
    val e2 = 1.0/6.0
    val e3 = 1.0/7.0
    
    d1.probabilityTable(4) should be(e1 +- tolerance)
    d2.probabilityTable(4) should be(e2 +- tolerance)
    d3.probabilityTable(4) should be(e3 +- tolerance)
  }
  
  "calculatesMapOfCountsForEachObservation" in {
    assert(d1.freqTable === Map(4 -> 1, 5 -> 1, 6 ->1))
    assert(d2.freqTable === Map(4 -> 1, 5 -> 2, 6 ->3))
	assert(d3.freqTable === Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 1))
  }
  
  "isAugmentableWithTraversableObject" in {
    val s1 = IndexedSeq(6)
    val d4 = d1 ++ s1
    
    assert(d4.supportSize === 3)
    d4.probabilityTable(6) should be(0.5 +- tolerance)
    assert(d4.freqTable === Map(4 -> 1, 5 -> 1, 6 ->2))
  }
  
  "overridesHashCodeAndEquals" in {
    val d1a = IndexedSeq(4,5,6).toEmpiricalTable
    val empSeq = IndexedSeq(1,2,3).toEmpiricalSeq

    assert(d1.canEqual(d2))
    assert(!d1.canEqual(empSeq))

    assert(d1.equals(d1a))
    assert(d1.hashCode === d1a.hashCode)
    
    assert(!d1.equals(d2))
    assert(!(d1.hashCode equals d2.hashCode))
  }
}
