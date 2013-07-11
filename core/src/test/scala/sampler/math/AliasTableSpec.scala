package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito
import scala.collection.mutable.Queue

@RunWith(classOf[JUnitRunner])
class AliasTableSpec extends Specification with Mockito {

  "Alias method" should {
    
    val rawProbSeq = Partition.fromWeights(IndexedSeq(0.1, 0.2, 0.3, 0.4))
  
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
      
      val anotherPartition = Partition.fromWeights(IndexedSeq(
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
    
    "remain functional when probability of zero entered" in {
      val zeroProbSpec = Partition.fromWeights(IndexedSeq(0.1, 0.2, 0, 0.3, 0.4))
  
      val zeroAlias = new AliasTable(zeroProbSpec)
      
      "produces correct probability table" in {
        val probs = zeroAlias.probability
        
        val tolerance = 1e-6
        
        (probs(0) must beCloseTo(0.5, tolerance)) and
        (probs(1) must beCloseTo(1.0, tolerance)) and
        (probs(2) must beCloseTo(0.0, tolerance)) and
        (probs(3) must beCloseTo(1.0, tolerance)) and
        (probs(4) must beCloseTo(0.5, tolerance))
      }
      
      "produces correct alias table" in {
        val expectedAlias = Array(4,0,4,0,3)
        
        val alias = zeroAlias.alias
        
        (alias(0) mustEqual expectedAlias(0)) and
        (alias(1) mustEqual expectedAlias(1)) and
        (alias(2) mustEqual expectedAlias(2)) and
        (alias(3) mustEqual expectedAlias(3)) and
        (alias(4) mustEqual expectedAlias(4))
      }
      
      "samples as expected" in {
    	  val rand = Random
    			  
    	  def sample(samples: List[Int], currentIt: Int, numIts: Int): List[Int] = {
    	    if(currentIt>=numIts) samples
            else {
    	      sample(samples.:+(zeroAlias.next(rand)), currentIt+1, numIts)
    	    }
    	  }
    	  
    	  val requiredIterations = 1000
    	  val sampledInts = sample(List(), 0, requiredIterations)

    	  val zero = sampledInts.count(_ == 0)
   		  val one = sampledInts.count(_ == 1)
   		  val two = sampledInts.count(_ == 2)
   		  val three = sampledInts.count(_ == 3)
   		  val four = sampledInts.count(_ == 4)
   		  
   		  (zero must beBetween(60, 140)) and
   		  (one must beBetween(160, 240)) and
   		  (two mustEqual 0) and
   		  (three must beBetween(260, 340)) and
   		  (four must beBetween(360, 440))
      }
    }
  }
}