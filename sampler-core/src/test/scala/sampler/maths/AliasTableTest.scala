package sampler.maths

import org.scalatest.Matchers
import org.scalatest.FreeSpec
import scala.collection.mutable.Queue

class AliasTableTest extends FreeSpec with Matchers {

  val rawProbSeq = IndexedSeq(0.1, 0.2, 0.3, 0.4)
  val myAlias = new AliasTable(rawProbSeq)
  
  val tolerance = 1e-6
  
  "Generates correct probability table" in {
    val result = myAlias.probability
    
    val expected = Array(0.4, 0.8, 1.0, 0.8)
    
    result(0) should be(expected(0) +- tolerance)
    result(1) should be(expected(1) +- tolerance)
    result(2) should be(expected(2) +- tolerance)
    result(3) should be(expected(3) +- tolerance)
  }
  
  "Generates correct alias table" in {
    val alias = myAlias.alias
      
    val expectedAlias = Array(3,3,0,2)
    
    assert(alias === expectedAlias)
  }
  
  "Returns correct index, when sampling from mocked object" in {
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
  
  "Generates correct probability table - complicated example" in {
    /*This was done by looking at the results of a more complicated example
    * from the original Java implementation
    * http://www.keithschwarz.com/interesting/code/?dir=alias-method
    */
    
    val probs = IndexedSeq(0.11, 0.05, 0.31, 0.17, 0.08, 0.19, 0.09)
          
    val result = new AliasTable(probs).probability

    result(0) should be(0.77 +- tolerance)
    result(1) should be(0.35 +- tolerance)
    result(2) should be(1.0 +- tolerance)
    result(3) should be(0.71 +- tolerance)
    result(4) should be(0.56 +- tolerance)
    result(5) should be(0.96 +- tolerance)
    result(6) should be(0.63 +- tolerance)
  }
  
  "Generates correct Alias table - complicated example" in {
    val probs = IndexedSeq(0.11, 0.05, 0.31, 0.17, 0.08, 0.19, 0.09)

    val generatedAlias = new AliasTable(probs).alias
    
    val expectedAlias = Array(2,2,0,2,3,3,5)
        
    assert(generatedAlias === expectedAlias)
  }
  
  "Accepts a Partition containing a zero probability" in {
    val zeroProbSpec = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
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
  
  "Samples correctly from Partition with a zero probability" in {
    val zeroProbSpec = IndexedSeq(0.1, 0.2, 0, 0.3, 0.4)
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