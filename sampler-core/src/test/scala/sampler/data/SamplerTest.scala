package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class SamplerTest extends AssertionsForJUnit {

  @Test def serialSamplingTest {
    val dist = Distribution.continually(1)
    
    val sampledList = SerialSampler.apply(dist)(new ConvergenceProtocol[Int](3, 1.0, 100000) with MaxMetric)
    
    val expectedList = List(1,1,1,1)
    
    assert(sampledList === expectedList)
  }
  
  @Test def samplerGivesCorrectSequenceFromParrallel {
    val dist = Distribution.continually(2)
    
    val sampledList = ParallelSampler.apply(dist)(new ConvergenceProtocol[Int](3, 1.0, 100000) with MaxMetric)
    
    val expectedList = List(2,2,2,2,2,2)
    
    assert(sampledList === expectedList)
  }
  
  @Test def serialSamplingStopsOnMaximum {
    val dist = Distribution.continually(1)
    
    val maxRetries = 7
    
    val sampledList = SerialSampler.apply(dist)(new ConvergenceProtocol[Int](3, 0.0, maxRetries) with MaxMetric)
    
    val expectedList = List(1,1,1,1,1,1,1,1)
    
    assert(sampledList === expectedList)
  }
  
  
  @Test def parallelSamplingStopsOnMaximum {
    val dist = Distribution.continually(1)
    
    val maxRetries = 7
    
    val sampledList = ParallelSampler.apply(dist)(new ConvergenceProtocol[Int](3, 0.0, maxRetries) with MaxMetric)
    
    val expectedList = List(1,1,1,1,1,1,1,1,1)
    
    assert(sampledList === expectedList)
  }
}