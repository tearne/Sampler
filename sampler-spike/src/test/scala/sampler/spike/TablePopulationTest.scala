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

class PopulationTest extends AssertionsForJUnit with MockitoSugar with ShouldMatchers {

  implicit var r: Random = _

  @Before 
  def initialise = {
    r = mock[Random]
  }

  @Test
  def sizeOfTablePopulation {
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
    when(r.nextInt(anyInt)).thenReturn(2,2,1)
    
	val p = new TablePopulation(Map("A" -> 1, "B" -> 9))
    
    val populations = p.remove(3)
    
    val samples = populations._1.toMap
    val remainder = populations._2.toMap
    
    assert(samples.getOrElse("A", 0) === 1)
    assert(samples.getOrElse("B", 0) === 2)
    assert(remainder.getOrElse("A", 0) === 0)
    assert(remainder.getOrElse("B", 0) === 7)
  }
  
  @Test
  def neverReplacesWhenSamplingWithoutReplacement {
    implicit val random = Random
    val p = new TablePopulation(Map("A" -> 1, "B" -> 9))(random)
    
    val populations = p.remove(10)
    val samples = populations._1.toMap
    
    assert(samples.getOrElse("A", 1) === 1)
  }

  @Test
  def failIfRequestToRemoveAnIllogicalNumber {
    val p = new TablePopulation(Map("A" -> 1, "B" -> 9))
    
    intercept[AssertionError] {
      p.remove(11)
    }

    intercept[AssertionError] {
    	p.remove(-1)
    }
  }
  
  @Test
  def addsTablePopulationsTogether1 {
    val p = new TablePopulation(Map("A" -> 1, "B" -> 9))
    val p2 = new TablePopulation(Map("A" -> 1, "B" -> 1))
    
    val newPop = p + p2
    val counts = newPop.toMap
    
    assert(newPop.size === 12)
    assert(counts.getOrElse("A", 0) === 2)
    assert(counts.getOrElse("B", 0) === 10)
  }
  
  @Test
  def addsTablePopulationsTogether2 {
	  val p = new TablePopulation(Map("A" -> 1, "B" -> 9))
	  val p2 = new TablePopulation(Map("B" -> 1, "C" -> 5))
	  
	  val newPop = p + p2
	  val counts = newPop.toMap
	  
	  assert(newPop.size === 16)
	  assert(counts.getOrElse("A", 0) === 1)
	  assert(counts.getOrElse("B", 0) === 10)
	  assert(counts.getOrElse("C", 0) === 5)
  }
}