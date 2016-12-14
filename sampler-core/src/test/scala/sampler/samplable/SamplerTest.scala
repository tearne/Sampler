package sampler.samplable

import org.scalatest.FreeSpec
import org.scalatest.junit.AssertionsForJUnit
import sampler.distribution.Distribution
import sampler.maths.Random

class SamplerTest extends FreeSpec with AssertionsForJUnit {

  val r = Random

  "Serial sampling converges on correct list" in {
    val dist = Distribution.always(1)

    val sampledList = SerialSampler.apply(dist, r)(new ConvergenceProtocol[Int](3, 1.0, 100000) with MaxMetric)

    val expectedList = List(1,1,1,1,1,1)

    assert(sampledList === expectedList)
  }

  "Sampling using parallel sampler gives correct list" in {
    val dist = Distribution.always(2)

    val sampledList = ParallelSampler.apply(dist, r)(new ConvergenceProtocol[Int](3, 1.0, 100000) with MaxMetric)

    val expectedList = List(2,2,2,2,2,2)

    assert(sampledList === expectedList)
  }

  "Serial sampling stops on max retries" in {
    val dist = Distribution.always(1)

    val maxRetries = 7

    val sampledList = SerialSampler.apply(dist, r)(new ConvergenceProtocol[Int](3, 0.0, maxRetries) with MaxMetric)

    val expectedList = List(1,1,1,1,1,1,1,1,1)

    assert(sampledList === expectedList)
  }


  "Parallel sampling stops on max retries" in {
    val dist = Distribution.always(1)

    val maxRetries = 7

    val sampledList = ParallelSampler.apply(dist, r)(new ConvergenceProtocol[Int](3, 0.0, maxRetries) with MaxMetric)
    val expectedList = List(1,1,1,1,1,1,1,1,1)

    assert(sampledList === expectedList)
  }

  "Serial sampling of a Normal distribution" in {
    implicit val r: Random = Random
    val dist = Distribution.uniform(0, 10)

    val sampledList = SerialSampler.apply(dist, r)(new ConvergenceProtocol[Int](1000, 0.001, 100000) with MaxMetric)

    fail("assertion pending")
  }
}