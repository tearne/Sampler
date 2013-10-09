package sampler.spike

import sampler.math.Random
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.Test
import org.mockito.Mockito._
import org.mockito.Mockito
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.scalatest.matchers.ShouldMatchers

class SetPopulationTest extends AssertionsForJUnit with MockitoSugar with ShouldMatchers {

  implicit var r: Random = _

  @Before 
  def initialise = {
    r = mock[Random]
  }
  @Test
  def samplesWithoutReplacement {
    when(r.nextInt(anyInt)).thenReturn(3)
    
	val p = new SetPopulation(IndexedSeq("A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))
    
    val newPops = p.remove(3)
    
    val samples = newPops._1.values
    val remaining = newPops._2.values
    
    assert(samples.count(_ == "A") === 0)
    assert(samples.count(_ == "B") === 3)
    assert(remaining.count(_ == "A") === 1)
    assert(remaining.count(_ == "B") === 6)
  }
  
  @Test
  def samplesWithReplacementWithCorrectProbabilities {
    implicit val random = Random
    val p = new SetPopulation(IndexedSeq("A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))(random)
    
    def countUp[T](pop: Population[T], n: Integer, acc: Map[String, Int]): Map[String, Int] = {
      if(n == 1000) acc
      else {
        val samples = p.remove(10)._1.values
        
        val a = samples.count(_ == "A") + acc.getOrElse("A", 0)
        val b = samples.count(_ == "B") + acc.getOrElse("B", 0)
        
        countUp(pop, n+1, Map("A" -> a, "B" -> b))
      }
    }
    
    val result = countUp(p, 0, Map("A" -> 0, "B" -> 0))
    
    result.getOrElse("A", 0) should be(1000 plusOrMinus 50)
    result.getOrElse("B", 0) should be(9000 plusOrMinus 50)
  }
  
  @Test
  def failIfSampleSizeBelowZeroOrTooLarge {
    val p = new SetPopulation(IndexedSeq("A", "B", "B", "B", "B", "B", "B", "B", "B", "B"))
    
    intercept[AssertionError] {
      p.remove(11)
    }

    intercept[AssertionError] {
    	p.remove(-1)
    }
  }
}