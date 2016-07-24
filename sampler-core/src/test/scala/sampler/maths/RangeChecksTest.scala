package sampler.maths

import sampler._

import org.scalatest.Matchers
import org.scalatest.FreeSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class RangeChecksTest extends FreeSpec 
    with Matchers
    with GeneratorDrivenPropertyChecks {

  "Individual probabilities" in {
    assert(0.0.isProbability)
    assert(1.0.isProbability)
    assert(!(1 + 1e-10).isProbability)
    assert(!(0 - 1e-10).isProbability)
    
    forAll{p: Double =>
      whenever(p >= 0 && p <= 1){ 
        assert(p.isProbability)
      }
    }
    
    forAll{p: Double =>
      whenever(p < 0 || p > 1){ 
        assert(!p.isProbability) 
      }
    }
  }
  
  "Traversable of probabilities" in {
     assert(Seq(0, 0.1, 0.2, 0.3, 0.4, 0.9, 0.9999, 1).areProbabilities)
     assert(!Seq(0, 0.1, 0.2, 0.3, 1 + 1e-10, 0.9, 0.9999, 1).areProbabilities)
  }
}