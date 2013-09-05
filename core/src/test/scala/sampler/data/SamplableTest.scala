package sampler.data

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import org.junit.Test

class SamplableTest extends AssertionsForJUnit {

  var instance: Samplable[Int] = _
  var instance2: Samplable[Int] = _
  var alwaysOne: Samplable[Int] = _
  
  @Before def initialise {
    
    instance = new Samplable[Int] {
	  val it = List(0,1,2,3,4,5,6,7,8,9).iterator
			
	  def sample(): Int = it.next()
	}
    
    instance2 = new Samplable[Int] {
      val it = List(0,1,2,3,4,5,6,7,8,9).iterator
      
      def sample(): Int = it.next()
    }
    
    alwaysOne = new Samplable[Int] {
      def sample = 1
    }
  }
  
  def append(previous: Seq[Int], s: Samplable[Int], requiredLength: Int): Seq[Int] = {
    if(previous.length == requiredLength) previous
	else append(previous.:+(s.sample()), s, requiredLength)
  }
  
  @Test def samplesValuesInOrder {
    val sampledSeq = append(Seq(), instance, 10)
			
	assert(sampledSeq === Seq(0,1,2,3,4,5,6,7,8,9))
  }
  
  @Test def sampleUntilLengthFive {
    val resultsSeq = instance.until(_.size == 5).sample
				
	val expectedSeq = IndexedSeq(0,1,2,3,4)
				
	assert(resultsSeq === expectedSeq)
  }
  
  @Test def sampleRepeatedlyUsingUntilEvenValue {
    val untilInstance = instance.until(_.last % 2 == 0)
				
	val seq1 = untilInstance.sample
	val seq2 = untilInstance.sample
	val seq3 = untilInstance.sample
				
	assert(seq1 === IndexedSeq(0))
	assert(seq2 === IndexedSeq(1,2))
	assert(seq3 === IndexedSeq(3,4))
  }
  
  @Test def filterForValuesGreaterThanTwo {
    val filtered = instance.filter(_ > 2)
	
    val sampleList = append(Seq(filtered.sample()), filtered, 7)
				
	assert(sampleList === Seq(3,4,5,6,7,8,9))
  }
  
  @Test def filtersForEvenNumbers {
    val filtered = instance.filter(_ % 2 == 0)
    
    val sampleList = append(Seq(), filtered, 5)
				
	assert(sampleList === Seq(0,2,4,6,8))
  }
  
  @Test def mapsValuesToDoubleValue {
    val mapped = instance.map(value => value * 2)
    
	val sampleList = append(Seq(), mapped, 10)
			
	assert(sampleList === List(0,2,4,6,8,10,12,14,16,18))
  }
  
  @Test def flatMap {
    // TODO
  }
  
  @Test def combinedTwoSamplablesWithProduct {
    def product(a: Int, b: Int) = a*b
		    
    val result = instance.combine(instance2)(product)
    
	val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(0,1,4,9,16))
  }
  
  @Test def addTwoSamplablesWithConvolvle {
    val result = instance.convolve(alwaysOne)
	
    val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(1,2,3,4,5))
  }
  
  @Test def subtractSamplableWithCrossCorrelate {
    val result = instance.crossCorrelate(alwaysOne)
		    
	val sampleList = append(Seq(), result, 5)
				
	assert(sampleList === Seq(-1,0,1,2,3))
  }
//  
//	"Samplable" should {
//		"have a Bernoilli trial object" in {
//		  import sampler.data.Empirical._
//		  import sampler.math.Probability
//
//		  "that consistently returns true when probability equals one" in {
//		    val model = Samplable.bernouliTrial(Probability(1))
//		    
//		    val result = (1 to 10).map(_ => model.sample)
//		    
//		    result.count(_ == true) mustEqual 10
//		  }
//		  
//		  "that consistently returns false when probability equals zero" in {
//		    val model = Samplable.bernouliTrial(Probability(0))
//		    
//		    val result = (1 to 10).map(_ => model.sample)
//		    
//		    result.count(_ == true) mustEqual 0
//		  }
//		  
//		  "that returns results in the correct proportion given supplied probabilities" in {
//		    val model = Samplable.bernouliTrial(Probability(0.8))
//		    
//		    val result = (1 to 1000).map(_ => model.sample)
//		    
//		    result.count(_ == true) must beBetween(750, 850)
//		  }
//		}
//		
//		"have a (fair) coin object which can be tossed / sampled" in {
//		  val model = Samplable.coinToss
//		  
//		  val result = (1 to 100).map(_ => model.sample)
//		  
//		  result.count(_ == true) must beBetween(40, 60)
//		}
//		
//		//Covariance and contravariance tests (for compilation only)
//		object PlayingWithVariance{
//			class Random2 extends Random {   // Should this be being used somewhere?
//				def nextThingey() = 12
//			}
//
//			class T
//			class S extends T
//			val isT: T = new S
//			
//			val t = new Samplable[T]{
//				def sample() = new T
//			}
//			val s = new Samplable[S]{
//				def sample() = new S
//			}
//			
//			val res1 = t.sample()
//			val res2 = s.sample()
//			val res3 = s.sample()
//			
//			val u: Samplable[T] = s
//			val w: Samplable[S] = s
//			
//			class U
//			val p: Samplable[U] = t.combine(s)((a:T, b:S) => new U)
//		}
//	}
//	
//	trait createInstance extends Scope {
//		val instance = new Samplable[Int] {
//			val it = List(0,1,2,3,4,5,6,7,8,9).iterator
//			
//			def sample(): Int = it.next()
//		}
//	}
//	
//	trait createTwoInstance extends Scope {
//	  val instance1 = new Samplable[Int] {
//		val it = List(1,1,1,1,1).iterator
//		def sample(): Int = it.next()
//	  }
//		    
//	  val instance2 = new Samplable[Int] {
//		val it = List(5,6,7,8,9).iterator
//		def sample(): Int = it.next()
//	  }
//	}
}