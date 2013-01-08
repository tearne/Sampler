package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito

@RunWith(classOf[JUnitRunner])
class AliasSpec extends Specification with Mockito {

  "Alias method" should {
    
    val rawProbSeq = IndexedSeq(0.1,0.2,0.3,0.4)
    val rand = mock[Random]
  
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
    
    "returns the correct index when sampling" in {
      rand.nextInt(4) returns (0,1,2,3)
      
      rand.nextDouble() returns (0.2, 0.9, 0.5, 0.1)
      
//      val s1 = myAlias.next
//      val s2 = myAlias.next
//      val s3 = myAlias.next
//      val s4 = myAlias.next
      
      val sampledResults = Array(myAlias.next, myAlias.next, myAlias.next, myAlias.next)
      
      val expectedResults = Array(0,3,2,3)
      
      (sampledResults(0) mustEqual expectedResults(0)) and
      (sampledResults(1) mustEqual expectedResults(1)) and
      (sampledResults(2) mustEqual expectedResults(2)) and
      (sampledResults(3) mustEqual expectedResults(3))
    }
  }
}