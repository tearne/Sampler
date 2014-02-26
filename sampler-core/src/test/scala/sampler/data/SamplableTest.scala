package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import sampler.math.Random
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import sampler.math.StatisticsComponent

class SamplableTest extends AssertionsForJUnit with MockitoSugar with ToSamplable {
  
  class MyParticle[T](a: Char)
    
  val p1 = new MyParticle('a')
  val p2 = new MyParticle('b')
  val p3 = new MyParticle('c')
  
  trait Colour
  object Red extends Colour { override def toString = "Red"}
  object Green extends Colour { override def toString = "Green"}
  object Blue extends Colour { override def toString = "Blue"}
  
  @Test
  def canDrawMoreThanTheNumberOfObjectTypesInBag {
    implicit val r = mock[Random]
    when(r.nextInt(anyInt)).thenReturn(599,350,350,250) // Blue, Green, Green Red
    
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
    
    val startSeq = IndexedSeq(Red, Red, Red, Green, Green)
    
    val(remaining, drawn) = startSeq.draw(2)

    assert(remaining.count(_ == Red) === 2)
    assert(remaining.count(_ == Green) === 1)

    assert(drawn.getOrElse(Red, 0) === 1)
    assert(drawn.getOrElse(Green, 0) === 1)
  }
  
  @Test def samplesFromImplicitMapCorrectly {
    implicit val r = mock[Random]
    
    when(r.nextInt(3)).thenReturn(2)
    when(r.nextInt(2)).thenReturn(1)
    
    val implicitMap = IndexedSeq(p1,p2,p3).map{
      case (particle) => particle -> 1
    }.toMap
    
    val result = implicitMap.draw(2)._2
    assert(result.getOrElse(p1, 0) === 0)
    assert(result.getOrElse(p2, 0) === 1)
    assert(result.getOrElse(p3, 0) === 1)
  }
  
  @Test
  def samplingFromDefinedMapCorrectly {
    implicit val r = mock[Random]
    
    when(r.nextInt(4)).thenReturn(2)
    when(r.nextInt(3)).thenReturn(2)
    when(r.nextInt(2)).thenReturn(0)
    
    val sMap = new SamplableMap(Map(p1 -> 1, p2 -> 2, p3 -> 1))

    val result = sMap.draw(3)._2
    
    assert(result.getOrElse(p1, 0) === 1)
    assert(result.getOrElse(p2, 0) === 1)
    assert(result.getOrElse(p3, 0) === 1)
  }
  
  @Test
  def lookingForBug {
    implicit val r = mock[Random]
    
    when(r.nextInt(3)).thenReturn(2)
    when(r.nextInt(2)).thenReturn(1)
    
    val implicitSeq = IndexedSeq(p1,p2,p3)
    
    val result = implicitSeq.draw(2)._2
   
    assert(result.getOrElse(p1, 0) === 0)
    assert(result.getOrElse(p2, 0) === 1)
    assert(result.getOrElse(p3, 0) === 1)
  }
}