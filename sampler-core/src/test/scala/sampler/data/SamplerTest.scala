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
	}