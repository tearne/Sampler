package sampler.spike

import sampler.math.Random
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito._
import org.mockito.Mockito
import org.scalatest.mock.MockitoSugar
import org.scalatest.matchers.ShouldMatchers

class PopulationTest extends AssertionsForJUnit with MockitoSugar with ShouldMatchers {

  implicit var r: Random = _

  @Before 
  def initialise = {
    r = mock[Random]
  }

  @Test
  def sizeOfTablePopulation = {
    val p = TablePopulation(Map("A" -> 1, "B" -> 9))
    assert(p.size === 10)
  }

  @Test
  def defaultSampleSizeIsOne {
    when(r.nextInt(10)).thenReturn(3)
    
	val p = new TablePopulation(Map("A" -> 1, "B" -> 9))
    
    val populations = p.remove()
    
    val samples = populations._1.toMap
    val remainder = populations._2.toMap
    
    assert(samples.getOrElse("A", 0) === 0)
    assert(samples.getOrElse("B", 0) === 1)
    assert(remainder.getOrElse("A", 0) === 1)
    assert(remainder.getOrElse("B", 0) === 8)
  }
  
  @Test
  def samplesWithoutReplacement {
    when(r.nextInt(10)).thenReturn(2)
    when(r.nextInt(9)).thenReturn(2)
    when(r.nextInt(8)).thenReturn(1)
    
	val p = new TablePopulation(Map("A" -> 1, "B" -> 9))
    
    val populations = p.remove(3)
    
    val samples = populations._1.toMap
    val remainder = populations._2.toMap
    
    assert(samples.getOrElse("A", 0) === 1)
    assert(samples.getOrElse("B", 0) === 2)
    assert(remainder.getOrElse("A", 0) === 0)
    assert(remainder.getOrElse("B", 0) === 7)
  }
  
//  @Test
//  def returnsRemainingMapWithoutReplacement {
//    when(r.nextInt(10)).thenReturn(1,1,1)
//    
//	val p = new Population(Map("A" -> 1, "B" -> 9))
//    
//    val remaining = p.sampleWithoutReplacement(3)._2
//    
//    assert(remaining.getOrElse("A", 0) === 0)
//    assert(remaining.getOrElse("B", 0) === 7)
//  }
//  
//  @Test
//  def neverReplacesWhenSamplingWithReplacement {
//    implicit val random = Random
//    val p = new Population(Map("A" -> 1, "B" -> 9))(random)
//    
//    (1 to 100).map{a =>
//      val samples = p.sampleWithoutReplacement(10)._1
//      
//      assert(samples.getOrElse("A", 1) === 1)
//    }
//  }
//  
//  @Test
//  def samplesWithReplacement {
//    when(r.nextInt(10)).thenReturn(3)
//    
//	val p = new Population(Map("A" -> 1, "B" -> 9))
//    
//    val samples = p.sampleWithReplacement(3)
//    
//    assert(samples.getOrElse("A", 0) === 0)
//    assert(samples.getOrElse("B", 0) === 3)
//  }
//  
//  @Test
//  def samplesWithReplacementWithCorrectProbabilities {
//    implicit val random = Random
//    val p = new Population(Map("A" -> 1, "B" -> 9))(random)
//    
//    def countUp[T](pop: Population[T], n: Integer, acc: Map[String, Int]): Map[String, Int] = {
//      if(n == 1000) acc
//      else {
//        val samples = p.sampleWithReplacement(10)
//        
//        val a = samples.getOrElse("A", 0) + acc.getOrElse("A", 0)
//        val b = samples.getOrElse("B", 0) + acc.getOrElse("B", 0)
//        
//        countUp(pop, n+1, Map("A" -> a, "B" -> b))
//      }
//    }
//    
//    val result = countUp(p, 0, Map("A" -> 0, "B" -> 0))
//    
//    result.getOrElse("A", 0) should be(1000 plusOrMinus 50)
//    result.getOrElse("B", 0) should be(9000 plusOrMinus 50)
//  }
//  
//  @Test
//  def failIfSampleSizeBelowZeroOrTooLarge {
//    val p = new Population(Map("A" -> 1, "B" -> 9))
//    
//    intercept[AssertionError] {
//      p.sampleWithoutReplacement(11)
//    }
//
//    intercept[AssertionError] {
//    	p.sampleWithoutReplacement(-1)
//    }
//    
//    intercept[AssertionError] {
//      p.sampleWithReplacement(11)
//    }
//    
//    intercept[AssertionError] {
//    	p.sampleWithReplacement(-1)
//    }
//  }
}