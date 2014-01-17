package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

class SamplerTest extends AssertionsForJUnit {

  @Test def samplerGivesCorrectSequence {
    val chunkSize = 3
    
    val dist = Distribution.continually(1)
    
    val ps = new ParallelSampler(chunkSize)
    val sampledList = ps.apply(dist)(_.length > 4)
    
    val expectedList = List(1,1,1,1,1,1)
    
    assert(sampledList === expectedList)
  }

  @Test def failsUnexpectedly {
    // TODO check failure due to parrallelisation
    
    val chunkSize = 3
    
    val dist = {
      val it = List(0,1,2,3,4,5,6,7,8,9).iterator
      Distribution(it.next)
    }
    
    val ps = new ParallelSampler(chunkSize)
    val sampledList = ps.apply(dist)(_.length > 4)
    
    val expectedList = List(0,1,2,3,4,5)
    
    println(sampledList)
    println(expectedList)
    
    assert(sampledList === expectedList)
  }
}