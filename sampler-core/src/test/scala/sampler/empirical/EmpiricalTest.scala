package sampler.empirical

import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.posNum

class EmpiricalTest extends FreeSpec with GeneratorDrivenPropertyChecks {
  def buildFrom[A](count: Int, probTab: Map[A, Double]) = new Empirical[A]{
    val observationCount = count
    val probabilityTable = probTab
  }
  
  val weightsMapGen = 
    arbitrary[List[Double]]
      .suchThat(_.sum > 0)
      .map{list =>
        val map = list.map(v => math.abs(v)).zipWithIndex.map(_.swap).toMap
        val totalWeight = map.values.sum
        map.mapValues(_ / totalWeight)
      }
  
  "Right tail probability of empty table" in {
    assert(0.0 === buildFrom(0, Map()).rightTail(null))
    assert(0.0 === buildFrom(0, Map()).rightTail(1))
  }
  
  "Right tail probability of simple map" in {
    val map = Map(1 -> 0.1, 2 -> 0.2, 3 -> 0.3, 4 -> 0.4)
    
    assert(buildFrom(3, map).rightTail(1) === 1)
    assert(buildFrom(3, map).rightTail(2) === 0.9)
    assert(buildFrom(3, map).rightTail(3) === 0.7)
    assert(buildFrom(3, map).rightTail(4) === 0.4)
  }
  
  "Right tail probability of generated map" in forAll(weightsMapGen, posNum[Int]){ (wtMap: Map[Int, Double], int: Int) =>
      val index = int % wtMap.size
      val sortedKeys = wtMap.keys.toIndexedSeq.sorted
      val item = sortedKeys.apply(index)
      val expected = sortedKeys.filter(_ >= item).map(wtMap).sum
      
      assert(expected == buildFrom(0, wtMap).rightTail(item))
  }
  
  "Left tail" in pending
  
  "Percentile" in pending
  
  "Percentile sequence" in pending
  
  "Mean" in pending
  
  "MeanDistanceTo" in pending
  
  "MaxDistanceTo" in pending
}