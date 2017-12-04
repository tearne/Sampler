package sampler.samplable

import org.scalatest.FreeSpec
import org.scalatest.mockito.MockitoSugar
import sampler.maths.Random
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.anyInt

import scala.collection.immutable.TreeMap

class WithoutReplacementTest extends FreeSpec with MockitoSugar {
  class MyParticle[T](a: Char)

  val p1 = new MyParticle('a')
  val p2 = new MyParticle('b')
  val p3 = new MyParticle('c')

  trait Colour
  object Red extends Colour
  object Green extends Colour
  object Blue extends Colour
  object Colour {
    // Lexicographic ordering
    implicit val ordering = Ordering.fromLessThan[Colour](_.toString < _.toString)
  }

  "Sampling from a map" in {
    val startBag = TreeMap(Blue -> 100, Green -> 200, Red -> 300)

    implicit val r = mock[Random]
    when(r.nextInt(anyInt)).thenReturn(50,350,350,250) // Blue, Red, Red, Green

    val sample = startBag.draw(4)

    assert(sample.remainder.getOrElse(Blue, 0) === 99)
    assert(sample.remainder.getOrElse(Green, 0) === 199)
    assert(sample.remainder.getOrElse(Red, 0) === 298)

    assert(sample.drawnCounts.getOrElse(Blue, 0) === 1)
    assert(sample.drawnCounts.getOrElse(Green, 0) === 1)
    assert(sample.drawnCounts.getOrElse(Red, 0) === 2)
  }

  "Sampling from a sequence" in {
    val startSeq = IndexedSeq(Red, Red, Red, Green, Green)

    implicit val r = mock[Random]
    when(r.nextInt(anyInt)).thenReturn(2,2) // Red, Green

    val sample = startSeq.draw(2)

    assert(sample.remainder.count(_ == Red) === 2)
    assert(sample.remainder.count(_ == Green) === 1)

    assert(sample.drawnCounts.getOrElse(Red, 0) === 1)
    assert(sample.drawnCounts.getOrElse(Green, 0) === 1)
  }

  "Sampling last item in indexedSeq" in {
    implicit val r = mock[Random]

    when(r.nextInt(3)).thenReturn(2)
    when(r.nextInt(2)).thenReturn(1)

    val implicitSeq = IndexedSeq(p1,p2,p3)

    val result = implicitSeq.draw(2).drawnCounts

    assert(result.getOrElse(p1, 0) === 0)
    assert(result.getOrElse(p2, 0) === 1)
    assert(result.getOrElse(p3, 0) === 1)
  }
}
