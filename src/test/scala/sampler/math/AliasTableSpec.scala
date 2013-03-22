package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito

@RunWith(classOf[JUnitRunner])
class AliasTableSpec extends Specification with Mockito {

  "Alias method" should {
    
    val rawProbSeq = IndexedSeq(0.1,0.2,0.3,0.4).map(v => Probability(v))
  
    val myAlias = new AliasTable(rawProbSeq)
    
    "return the correct probability table" in {
    	val probs = myAlias.probability
    	
    	val expectedProbs = Array(0.4, 0.8, 1.0, 0.8)
    	
    	val tolerance = 1e-6
    	
    	(probs(0) must beCloseTo(expectedProbs(0), tolerance)) and
    	(probs(1) must beCloseTo(expectedProbs(1), tolerance)) and 
    	(probs(2) must beCloseTo(expectedProbs(2), tolerance)) and
    	(probs(3) must beCloseTo(expectedProbs(3), tolerance))
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
      val r = mock[Random]
      
      r.nextInt(4) returns (0,1,2,3)
      
      r.nextDouble() returns (0.2, 0.9, 0.5, 0.1)
      
      val sampledResults = Array(myAlias.next(r), myAlias.next(r), myAlias.next(r), myAlias.next(r))
      
      val expectedResults = Array(0,3,2,3)
      
      (sampledResults(0) mustEqual expectedResults(0)) and
      (sampledResults(1) mustEqual expectedResults(1)) and
      (sampledResults(2) mustEqual expectedResults(2)) and
      (sampledResults(3) mustEqual expectedResults(3))
    }
    
    "return the correct tables in a more complicated example" in {
      /*This was done by looking at the results of a more complicated example
       * from the original Java implementation
       * http://www.keithschwarz.com/interesting/code/?dir=alias-method
       */
      
      val moreProbs = IndexedSeq(
          0.11, 0.05, 0.31, 0.17, 0.08, 0.19, 0.09).map(v => Probability(v))
      
      val biggerAlias = new AliasTable(moreProbs)
      
      "probability table is correct" in {
        val probs = biggerAlias.probability
        
        val tolerance = 1e-6
        
        (probs(0) must beCloseTo(0.77, tolerance)) and
        (probs(1) must beCloseTo(0.35, tolerance)) and
        (probs(2) must beCloseTo(1.0, tolerance)) and
        (probs(3) must beCloseTo(0.71, tolerance)) and
        (probs(4) must beCloseTo(0.56, tolerance)) and
        (probs(5) must beCloseTo(0.96, tolerance)) and
        (probs(6) must beCloseTo(0.63, tolerance))        
      }
      
      "alias table is correct" in {
    	  val expectedAlias = Array(2,2,0,2,3,3,5)
        
    	  val generatedAlias = biggerAlias.alias
    	  
    	  generatedAlias mustEqual expectedAlias
      }
    }
    
        "throw an exception" in {
	    "if the probabilities don't add to one" in {
	      val probabilities = IndexedSeq(0.1, 0.2, 0.3, 0.3).map(v => Probability(v))
	    			
	      new AliasTable(probabilities) must throwAn[AssertionError]
	    }
	}
    
    "not throw an exception" in {
	    "if the probability sum is not equal to one because of a rounding error" in {
	      val seventh = 1.0/7.0
	      val forteenth = 1.0/14.0
	      
	      val probabilities = IndexedSeq(seventh, seventh, seventh, seventh, seventh, seventh, forteenth, forteenth).map(v => Probability(v))
	      // Sum = 0.9999999999999998
	      
	      new AliasTable(probabilities) must not(throwAn[AssertionError])
	    }
    }
  }
}