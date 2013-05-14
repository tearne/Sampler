package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito
import scala.collection.mutable.Queue

@RunWith(classOf[JUnitRunner])
class AliasTableSpec extends Specification with Mockito {

  "Alias method" should {
    
    val rawProbSeq = Partition(IndexedSeq(0.1, 0.2, 0.3, 0.4))
  
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
    
    "mock test" in {
    	val r = mock[scala.util.Random]
    	r.nextDouble() returns 3.0
    	r.nextDouble() === 3.0
    }
    
    "returns the correct index when sampling" in {
    	
    	//TODO this is a bit excessive, but can't get the mocking to work
    	val r = new Random{
    		val ints = Queue(0,1,2,3)
    		val doubles = Queue(0.2, 0.9, 0.5, 0.1)
    		override def nextInt(i: Int) = {
    			assert(i == 4)
    			ints.dequeue
    		}
    		override def nextDouble() = doubles.dequeue
    	}
    	
//    val r = mock[scala.util.Random]
//    r.nextInt(4).returns(0,1,2,3)
//    r.nextDouble().returns(0.2, 0.9, 0.5, 0.1)
      
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
      
      val anotherPartition = Partition(IndexedSeq(
          0.11, 0.05, 0.31, 0.17, 0.08, 0.19, 0.09))
      
      val biggerAlias = new AliasTable(anotherPartition)
      
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
    
    //TODO put this in a Parition spec
    "throw an exception" in {
	    "if the probabilities don't add to one" in {
	      val partition = Partition(IndexedSeq(0.1, 0.2, 0.3, 0.3))
	    			
	      new AliasTable(partition) must throwAn[AssertionError]
	    }
	}
    
    //TODO put this in a Parition spec
    "not throw an exception" in {
	    "if the probability sum is not equal to one because of a rounding error" in {
	      val seventh = 1.0/7.0
	      val forteenth = 1.0/14.0
	      
	      val probabilities = Partition(IndexedSeq(seventh, seventh, seventh, seventh, seventh, seventh, forteenth, forteenth))
	      // Sum = 0.9999999999999998
	      
	      new AliasTable(probabilities) must not(throwAn[AssertionError])
	    }
    }
  }
}