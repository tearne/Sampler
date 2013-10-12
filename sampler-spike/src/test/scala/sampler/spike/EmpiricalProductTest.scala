package sampler.spike

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.matchers.ShouldMatchers
import org.junit.Test
import sampler.Implicits._
import sampler.math.StatisticsComponent
import org.junit.Before
import sampler.data.EmpiricalSeq

class EmpiricalProductTest extends AssertionsForJUnit with ShouldMatchers with StatisticsComponent {

  var es1: EmpiricalSeq[Double] = _
  var es2: EmpiricalSeq[Double] = _
  var es3: EmpiricalSeq[Double] = _
  
  @Before def initialise() = {
    es1 = IndexedSeq[Double](1,2,3).toEmpiricalSeq
    es2 = IndexedSeq[Double](4,5,6).toEmpiricalSeq
    es3 = IndexedSeq[Double](7,8,9).toEmpiricalSeq
  }
  
  @Test def productAnswersCorrectWhenOnlyOneElement = {
    val ep = new EmpiricalProduct(IndexedSeq(es1))
    
    assert(es1.size === ep.size)
    assert(mean(es1) === ep.expectation.head)
  }
  
  @Test def attributesOfATwoProductImplementation {
    val ep = new EmpiricalProduct(IndexedSeq(es1, es2))
    
    assert(ep.size === 9)
    assert(ep.expectation === IndexedSeq(2.0,5.0))
  }
  
  @Test def threeProductImplementation {
    val ep = new EmpiricalProduct(IndexedSeq(es1, es2, es3))
    
    assert(ep.size === 27)
    assert(ep.expectation === IndexedSeq(2.0,5.0,8.0))
  }
  
  // Test is not fully implemented, functionality not fully defined
  @Test def distanceTo {
    val ep1 = new EmpiricalProduct(IndexedSeq(es1, es1))
    val ep2 = new EmpiricalProduct(IndexedSeq(es2, es3))
    
    assert(ep1.distTo(ep2) === 6.0)
  }
}