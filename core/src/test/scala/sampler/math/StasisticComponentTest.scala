package sampler.math

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import sampler.data.Empirical._
import org.scalatest.matchers.ShouldMatchers

class StasisticComponentTest extends AssertionsForJUnit with StatisticsComponent with ShouldMatchers {

  implicit val r = Random
  val tolerance = 1e-8
  
  @Test def calculatesEmpiricalSeqMean {
    val empSeq = IndexedSeq[Double](1,2,3,4).toEmpiricalSeq
	
    mean(empSeq) should be(2.5 plusOrMinus tolerance)
  }
  
  @Test def calculatesEmpiricalTableMean {
    val empTable = IndexedSeq[Double](1,2,3,4).toEmpiricalTable
    
    mean(empTable) should be(2.5 plusOrMinus tolerance)
  }

  @Test def calculatesEmpiricalWeightedMean {
    val empWeight = Map[Double, Double](
		(1, 0.25),
		(2, 0.25),
		(3, 0.25),
		(4, 0.25)
      ).toEmpiricalWeighted

    mean(empWeight) should be(2.5 plusOrMinus tolerance)
  }
  
  @Test def calcualatesAbsoluteDifferenceMetric {
    val instance1 = IndexedSeq[Double](1,2,3).toEmpiricalSeq // mean 2
	val instance2 = IndexedSeq[Double](4,5,6).toEmpiricalSeq // mean 5
			
	assert(meanDistance(instance1, instance2) === 3)
  }
  
  @Test def calculatesMaximumDifferenceMetric {
    val instance1 = IndexedSeq(1,2,3,4).toEmpiricalSeq 
	val instance2 = IndexedSeq(1,2,2,2).toEmpiricalSeq // biggest distance 4
			
	maxDistance(instance1, instance2) should be(0.5 plusOrMinus tolerance)
  }
}