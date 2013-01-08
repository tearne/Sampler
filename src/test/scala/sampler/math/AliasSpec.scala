package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AliasSpec extends Specification {

  "Alias method" should {
    
    val rawProbSeq = IndexedSeq(0.1,0.2,0.3,0.4)
    val rand = new Random
  
    val myAlias = new Alias(rawProbSeq, rand)
    
    "return the correct probability table" in {
    	val probs = myAlias.probability
    	
    	val expectedProbs = Array(0.4, 0.8, 1.0, 0.8)
    	
    	val tolerance = 1e-6
    	
    	probs(0) must beCloseTo(expectedProbs(0), tolerance)
    	probs(1) must beCloseTo(expectedProbs(1), tolerance)
    	probs(2) must beCloseTo(expectedProbs(2), tolerance)
    	probs(3) must beCloseTo(expectedProbs(3), tolerance)
    }
    
    "return the correct Alias table" in {
      val alias = myAlias.alias
      
      val expectedAlias = Array(3,3,0,2)
      
      (alias(0) mustEqual expectedAlias(0)) and
      (alias(1) mustEqual expectedAlias(1)) and
      (alias(2) mustEqual expectedAlias(2)) and
      (alias(3) mustEqual expectedAlias(3))
    }
    
    "returns the correct index when sampling" in todo
    
    "do some error checking" in todo
  }
}