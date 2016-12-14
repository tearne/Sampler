package sampler.empirical

import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.posNum

class EmpiricalTest extends FreeSpec with GeneratorDrivenPropertyChecks {
  def buildFromProbTab[A](count: Int, probTab: Map[A, Double]) = new Empirical[A]{
    val observationCount = count
    val probabilityTable = probTab
  }
  
  val weightsMapGen = 
    arbitrary[List[Double]]
      .suchThat(l => l.sum > 0)
      .map{list =>
        val map = list.map(v => math.abs(v)).zipWithIndex.map(_.swap).toMap
        val totalWeight = map.values.sum
        map.mapValues(_ / totalWeight)
      }
  
  "Right tail of empty table" in {
    assert(0.0 === buildFromProbTab(0, Map()).rightTail(null))
    assert(0.0 === buildFromProbTab(0, Map()).rightTail(1))
  }
  
  "Right tail of uniform outcomes" in {
    val empirical = buildFromProbTab(4, Map(
      1 -> 0.25, 2 -> 0.25, 3 -> 0.25, 4 -> 0.25     
    ))
    
    assert(empirical.rightTail(1) === 1)
    assert(empirical.rightTail(2) === 0.75)
    assert(empirical.rightTail(3) === 0.5)
    assert(empirical.rightTail(4) === 0.25)
    assert(empirical.rightTail(5) === 0)
  }
  
  "Right tail of weighted outcomes" in {
    val map = Map(1 -> 0.1, 2 -> 0.2, 3 -> 0.3, 4 -> 0.4)
    
    assert(buildFromProbTab(3, map).rightTail(1) === 1)
    assert(buildFromProbTab(3, map).rightTail(2) === 0.9)
    assert(buildFromProbTab(3, map).rightTail(3) === 0.7)
    assert(buildFromProbTab(3, map).rightTail(4) === 0.4)
  }
  
  "Right tail of generated map" in forAll(weightsMapGen, posNum[Int]){ (wtMap, int) =>
      val index = int % wtMap.size
      val sortedKeys = wtMap.keys.toIndexedSeq.sorted
      val item = sortedKeys.apply(index)
      val expected = sortedKeys.filter(_ >= item).map(wtMap).sum
      
      assert(expected == buildFromProbTab(0, wtMap).rightTail(item))
  }
  
  "Percentile" in pending
  
  "Percentile sequence" in pending
  
  "Mean" in pending
  
  "MeanDistanceTo" in pending
  
  "MaxDistanceTo" in pending
  
//  "Calculates empirical sequence mean" in {
//    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
//	
//    mean(empSeq) should be(2.5 +- tolerance)
//  }
//  
//  "Calculates empirical table mean" in {
//    val empTable = IndexedSeq[Double](1,2,3,4).toEmpiricalTable
//    
//    mean(empTable) should be(2.5 +- tolerance)
//  }
//
//  "Quantiles from an empirical sequence" in {
//    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
//    
//    assert(quantiles(empSeq, Seq(0.25, 0.5, 0.75)) === Seq(1.0,2.0,3.0))
//  }
//  
//  "Quantiles from an empirical table" in {
//    val empTable = IndexedSeq[Double](1,2,2,3,4).toEmpiricalTable
//    
//    assert(quantiles(empTable, Seq(0.25, 0.5, 0.75)) === Seq(2.0,2.0,3.0))
//  }
//  
//  "Quantiles from an empirical sequence of 12 primes" in {
//    val empSeq = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27,31,37).toEmpiricalSeq
//
//    assert(quantiles(empSeq, Seq(0.25, 0.5, 0.75)) === Seq(5.0,13.0,23.0))
//  }
//
//  "Quantiles from an empirical table of 11 primes" in {
//	val empTable = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27,31).toEmpiricalSeq
//	
//    assert(quantiles(empTable, Seq(0.25, 0.5, 0.75)) === Seq(5.0,13.0,23.0))
//  }
//
//  "Quantiles from an empirical sequence of 10 primes" in {
//	val empSeq = IndexedSeq[Double](2,3,5,7,11,13,17,19,23,27).toEmpiricalSeq
//	
//    assert(quantiles(empSeq, Seq(0.25, 0.5, 0.75)) === Seq(5.0,11.0,19.0))
//  }
//
//  "Quantiles from an empirical talbe of 9 primes" in {
//	val empTable = IndexedSeq[Double](2,3,5,7,11,13,17,19,23).toEmpiricalSeq
//	
//    assert(quantiles(empTable, Seq(0.25, 0.5, 0.75)) === Seq(5.0,11.0,17.0))
//  }
//  
//  "Quantile accepts sequence of length 1" in {
//    val empSeq = IndexedSeq[Double](1.0).toEmpiricalSeq
//    
//    assert(quantiles(empSeq, Seq(0.25, 0.5, 0.75)) === Seq(1.0,1.0,1.0))
//  }
//  
//  "Error when sequence of lenght zero to quantile" in {
//    val empSeq = IndexedSeq[Double]().toEmpiricalSeq
//    
//    intercept[AssertionError] {
//      quantiles(empSeq, Seq(0.25))
//    }
//  }
//  
//  "Exception when negative probability supplied" in {
//    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
//    
//    intercept[RangeException[Double]] {
//      quantiles(empSeq, Seq(0.25, -0.5))
//    }
//  }  
//
//  "Exception when probability greater than one supplied" in {
//    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
//    
//    intercept[RangeException[Double]] {
//      quantiles(empSeq, Seq(0.25, 5.0))
//    }
//  }
//  
//  "calcualatesAbsoluteDifferenceMetric" in {
//    val instance1 = IndexedSeq[Double](1,2,3).toEmpiricalSeq // mean 2
//	val instance2 = IndexedSeq[Double](4,5,6).toEmpiricalSeq // mean 5
//			
//	assert(meanDistance(instance1, instance2) === 3)
//  }
//  
//  "calculatesMaximumDifferenceMetric" in {
//    val instance1 = IndexedSeq(1,2,3,4).toEmpiricalSeq 
//	val instance2 = IndexedSeq(1,2,2,2).toEmpiricalSeq // biggest distance 4
//			
//	maxDistance(instance1, instance2) should be(0.5 +- tolerance)
//  }
  
}