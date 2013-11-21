package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import sampler.math.Random
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.junit.Test

class SamplableTest extends AssertionsForJUnit with MockitoSugar with ToSamplable {
  
  @Test
  def canDrawMoreThanTheNumberOfObjectTypesInBag {
    implicit val r = mock[Random]
    when(r.nextInt(anyInt)).thenReturn(599,350,350,250) // Blue, Green, Green Red
    
    trait Colour
    object Red extends Colour { override def toString = "Red"}
    object Green extends Colour { override def toString = "Green"}
    object Blue extends Colour { override def toString = "Blue"}
    
    val startBag = Map(Red -> 300, Green -> 200, Blue -> 100)
    
	val(remaining, drawn) = startBag.draw(4)
	
	assert(remaining.getOrElse(Red, 0) === 299)
	assert(remaining.getOrElse(Green, 0) === 198)
	assert(remaining.getOrElse(Blue, 0) === 99)
	
	assert(drawn.getOrElse(Red, 0) === 1)
	assert(drawn.getOrElse(Green, 0) === 2)
	assert(drawn.getOrElse(Blue, 0) === 1)
  }
  
  @Test
  def samplesFromSequenceOfObjects {
    implicit val r = mock[Random]
    when(r.nextInt(anyInt)).thenReturn(2,2) // Red, Green
    
    trait Colour
    object Red extends Colour { override def toString = "Red"}
    object Green extends Colour { override def toString = "Green"}
    object Blue extends Colour { override def toString = "Blue"}
    
    val startSeq = IndexedSeq(Red, Red, Red, Green, Green)
    
    println(startSeq.draw(2))
    
    val(remaining, drawn) = startSeq.draw(2)

    assert(remaining.count(_ == Red) === 2)
    assert(remaining.count(_ == Green) === 1)

    assert(drawn.getOrElse(Red, 0) === 1)
    assert(drawn.getOrElse(Green, 0) === 1)
  }
}