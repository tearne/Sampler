package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import sampler.math.Random
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar
import org.junit.Test
import sampler.math.StatisticsComponent

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
    
    val(remaining, drawn) = startSeq.draw(2)

    assert(remaining.count(_ == Red) === 2)
    assert(remaining.count(_ == Green) === 1)

    assert(drawn.getOrElse(Red, 0) === 1)
    assert(drawn.getOrElse(Green, 0) === 1)
  }
  
  @Test
  def lookingForBug {
    // TODO not an actual test - delete me
    implicit val r = Random
    class MyParticle[T](i: T) {
      override def toString = i.toString
    }
    
    val p1 = new MyParticle(1)
    val p2 = new MyParticle(2)
    val p3 = new MyParticle(3)
    
    val oneOfEach = Seq(p1,p2,p3).map{
      case (particle) => particle -> 1
    }.toMap
    
    // TODO fix not working implicit
//    val res = oneOfEach.draw(2)._2
    
    val sMap = new SamplableMap(oneOfEach)
    
    // TODO third particle never selected - fix this
    (0 to 100).map{_ =>
      val result = sMap.draw(2)._2
      if(result.keySet.contains(p3)) println("Contains 3")
    }
    
  }
}