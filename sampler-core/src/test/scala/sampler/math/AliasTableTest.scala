package sampler.math

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import org.scalatest.mock.MockitoSugar
import org.scalatest.Matchers

class AliasTableTest extends AssertionsForJUnit with Matchers with MockitoSugar {

  val rawProbSeq = Partition.fromWeights(IndexedSeq(0.1, 0.2, 0.3, 0.4))
  val myAlias = new AliasTable(rawProbSeq)
  
  val tolerance = 1e-6
  
  @Test def generatesCorrectProbabilityTable {
    val probs = myAlias.probability
    	
    val expectedProbs = Array(0.4, 0.8, 1.0, 0.8)
    
    probs(0) should be(expectedProbs(0) +- tolerance)
    probs(1) should be(expectedProbs(1) +- tolerance)
    probs(2) should be(expectedProbs(2) +- tolerance)
    probs(3) should be(expectedProbs(3) +- tolerance)
  }
  
  @Test def generatesCorrectAliasTable {
    val alias = myAlias.alias
      
    val expectedAlias = Array(3,3,0,2)
    
    assert(alias === expectedAlias)
  }
  
  @Test def returnsCorrectIndexWhenSamplingFromMockedObject {
    import scala.collection.mutable.Queue
    val r = new Random{
      val ints = Queue(0,1,2,3)
      val doubles = Queue(0.2, 0.9, 0.5, 0.1)
      override def nextInt(i: Int) = {
        assert(i == 4)
        ints.dequeue
      }
      override def nextDouble() = doubles.dequeue
    }
    
    val sampledResults = Array(myAlias.next(r), myAlias.next(r), myAlias.next(r), myAlias.next(r))
      
    val expectedResults = Array(0,3,2,3)
    
    assert(sampledResults === expectedResults)
  }
  
  @Test def generatesCorrectProbabilityTableComplicatedExample {
	/*This was done by looking at the results of a more complicated example
	* from the original Java implementation
	* http://www.keithschwarz.com/interesting/code/?dir=alias-method
	*/
    
    val anotherPartition = Partition.fromWeights(IndexedSeq(
          0.11, 0.05, 0.31, 0.17, 0.08, 0.19, 0.09))
          
    val probs = new AliasTable(anotherPartition).probability

    probs(0) should be(0.77 +- tolerance)
    probs(1) should be(0.35 +- tolerance)
    probs(2) should be(1.0 +- tolerance)
    probs(3) should be(0.71 +- tolerance)
    probs(4) should be(0.56 +- tolerance)
    probs(5) should be(0.96 +- tolerance)
    probs(6) should be(0.63 +- tolerance)
  }
  
  @Test def generatesCorrectAliasTableComplicatedExample {
    val anotherPartition = Partition.fromWeights(IndexedSeq(
          0.11, 0.05, 0.31, 0.17, 0.08, 0.19, 0.09))

    val generatedAlias = new AliasTable(anotherPartition).alias
    
    val expectedAlias = Array(2,2,0,2,3,3,5)
        
    assert(generatedAlias === expectedAlias)
  }
  
  @Test def acceptsPartitionWithValueOfZero {
    val zeroProbSpec = Partition.fromWeights(IndexedSeq(0.1, 0.2, 0, 0.3, 0.4))
    val zeroAlias = new AliasTable(zeroProbSpec)
        
    val probs = zeroAlias.probability
    val alias = zeroAlias.alias
    
    val expectedAlias = Array(4,0,4,0,3)
    
    probs(0) should be(0.5 +- tolerance)
    probs(1) should be(1.0 +- tolerance)
    probs(2) should be(0.0 +- tolerance)
    probs(3) should be(1.0 +- tolerance)
    probs(4) should be(0.5 +- tolerance)

    assert(alias === expectedAlias)
  }
  
  @Test def samplesCorrectFromPartitionWithZero {
    val zeroProbSpec = Partition.fromWeights(IndexedSeq(0.1, 0.2, 0, 0.3, 0.4))
    val zeroAlias = new AliasTable(zeroProbSpec)
    
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

    val errorRange = 40
    
    zero should be(100 +- errorRange)
    one should be(200 +- errorRange)
    two should be(0 +- errorRange)
    three should be(300 +- errorRange)
    four should be(400 +- errorRange)
  }
}