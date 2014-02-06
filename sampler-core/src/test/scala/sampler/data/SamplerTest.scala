package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class SamplerTest extends AssertionsForJUnit {

  @Test def serialSamplingTest {
    val dist = Distribution.continually(1)
    
    val ss = new SerialSampler
    val sampledList = ss.apply(dist)(new ConvergenceProtocol[Int](5, 1.0) with MaxMetric)
    
    val expectedList = List(1,1,1,1,1,1)
    
    assert(sampledList === expectedList)
  }
  
  @Test def samplerGivesCorrectSequence {
    val dist = Distribution.continually(2)
    
    val ps = new ParallelSampler
    val sampledList = ps.apply(dist)(new ConvergenceProtocol[Int](3, 1.0) with MaxMetric)
    
    val expectedList = List(2,2,2,2,2,2)
    
    assert(sampledList === expectedList)
  }
}