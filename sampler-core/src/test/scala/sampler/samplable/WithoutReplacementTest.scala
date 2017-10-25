package sampler.samplable

import org.scalatest.FreeSpec
import org.scalatest.mockito.MockitoSugar
import sampler.maths.Random
import org.mockito.Mockito._
import org.mockito.Matchers._

import scala.collection.immutable.TreeMap

class WithoutReplacementTest extends FreeSpec with MockitoSugar {
  class MyParticle[T](a: Char)

  val p1 = new MyParticle('a')
  val p2 = new MyParticle('b')
  val p3 = new MyParticle('c')

  trait Colour
  object Red extends Colour { override def toString = "Red"}
  object Green extends Colour { override def toString = "Green"}
  object Blue extends Colour { override def toString = "Blue"}

  "Can draw at least one of each object type in bag" in {
    val startBag = TreeMap(Red -> 300, Green -> 200, Blue -> 100)

    implicit val r = mock[Random]
    when(r.nextInt(anyInt)).thenReturn(599,350,350,250) // Will be Blue, Green, Green Red

    val sample = startBag.draw(4)

    assert(sample.remainder.getOrElse(Red, 0) === 299)
    assert(sample.remainder.getOrElse(Green, 0) === 198)
    assert(sample.remainder.getOrElse(Blue, 0) === 99)

    assert(sample.drawnCounts.getOrElse(Red, 0) === 1)
    assert(sample.drawnCounts.getOrElse(Green, 0) === 2)
    assert(sample.drawnCounts.getOrElse(Blue, 0) === 1)
    }
}
