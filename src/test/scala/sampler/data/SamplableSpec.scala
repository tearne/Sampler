package sampler.data

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math._
import org.specs2.specification.Scope

@RunWith(classOf[JUnitRunner])
class SamplableSpec extends Specification {
  implicit val rf = RandomFactory

	"Samplable" should {
		"have flatMap" in todo	//TODO
		
		"sample mocked values in order" in new createInstance {
			
			def append(previous: List[Int]): List[Int] = {
				if(previous.length == 10) previous
				else append(previous.:+(instance.sample()))
			}
			val sampleList = append(List(instance.sample()))
			
			val expectedList = List(0,1,2,3,4,5,6,7,8,9)
			
			sampleList mustEqual expectedList
		}
		
		"have sampleUntil method" in {
			"sample until the value 5 is sampled" in new createInstance {
				val resultsSeq = instance.until(_.size == 5).sample
				
				val expectedSeq = IndexedSeq(0,1,2,3,4)
				
				resultsSeq mustEqual expectedSeq
			}
			
			"sample until last number is even" in new createInstance {
				val untilInstance = instance.until(_.last % 2 == 0)
				
				val seq1 = untilInstance.sample
				val seq2 = untilInstance.sample
				val seq3 = untilInstance.sample
				
				(seq1 mustEqual IndexedSeq(0)) and
				(seq2 mustEqual IndexedSeq(1,2)) and
				(seq3 mustEqual IndexedSeq(3,4))
			}
		}
		
		"have a map method" in {
		  "where all values in Samplable can be doubled" in new createInstance {
		    val mappedInstance = instance.map(value => value * 2)
		    
		    def append(previous: List[Int]): List[Int] = {
				if(previous.length == 10) previous
				else append(previous.:+(mappedInstance.sample()))
			}
		    
			val sampleList = append(List(mappedInstance.sample()))
			
			val expectedList = List(0,2,4,6,8,10,12,14,16,18)
			
			sampleList mustEqual expectedList
		  }
		}
		
		"have filter method" in {
			"always sample greater than 2" in new createInstance{
				val newSamplable = instance.filter(_ > 2)
				
				def append(previous: List[Int]): List[Int] = {
					if(previous.length == 7) previous
					else append(previous.:+(newSamplable.sample()))
				}
				val sampleList = append(List(newSamplable.sample()))
				
				sampleList mustEqual List(3,4,5,6,7,8,9)
			}
			
			"always sample even numbers" in new createInstance {
				val newSamplable = instance.filter(_ % 2 == 0)

				def append(previous: List[Int]): List[Int] = {
					if(previous.length == 5) previous
					else append(previous.:+(newSamplable.sample()))
				}
				val sampleList = append(List(newSamplable.sample()))
				
				sampleList mustEqual List(0,2,4,6,8)
			}
		}
		
		"have a combine method" in {
		  "which can combine two samplables with a mutliplier" in new createInstance{
		    def product(a: Int, b: Int) = a*b
		    
		    val doubler = new Samplable[Int] {
		    	val it = List(0,1,2,3,4).iterator
			
				def sample(): Int = it.next()
		    }
		    
		    val result = doubler.combine(instance, product)
		    
		    def append(previous: List[Int]): List[Int] = {
					if(previous.length == 5) previous
					else append(previous.:+(result.sample()))
				}
		    val sampleList = append(List(result.sample()))
				
			sampleList mustEqual List(0,1,4,9,16)
		  }
		  
		  "is used to add two Samplables together in convolve" in new createTwoInstance {    
		    val result = instance1.convolve(instance2)
		    
		    def append(previous: List[Int]): List[Int] = {
					if(previous.length == 5) previous
					else append(previous.:+(result.sample()))
				}
		    val sampleList = append(List(result.sample()))
				
			sampleList mustEqual List(6,7,8,9,10)
		  }
		  
		  "is used to substract on Samplable from another in cross-correlate" in new createTwoInstance{
		    val result = instance2.crossCorrelate(instance1)
		    
		    def append(previous: List[Int]): List[Int] = {
					if(previous.length == 5) previous
					else append(previous.:+(result.sample()))
				}
		    val sampleList = append(List(result.sample()))
				
			sampleList mustEqual List(4,5,6,7,8)
		  }
		}
		
		"have a Bernoilli trial object" in {
		  
		  import sampler.data.Empirical._
		  import sampler.math.Probability

		  implicit val r = rf.newRandom
		  
		  "that returns only true when all probabilities equal one" in {
		    val probs = Seq(1, 1, 1).map(value => Probability(value))
		    
		    val model = Samplable.bernouliTrial(probs.toEmpiricalSeq)
		    
		    val result = (1 to 10).map(_ => model.sample)
		    
		    result.count(_ == true) mustEqual 10
		  }
		  
		  "that returns only false when all probabilities equal zero" in {
		    val probs = Seq(0, 0, 0).map(value => Probability(value))
		    
		    val model = Samplable.bernouliTrial(probs.toEmpiricalSeq)
		    
		    val result = (1 to 10).map(_ => model.sample)
		    
		    result.count(_ == true) mustEqual 0
		  }
		  
		  "that returns results in the correct proportion given supplied probabilities" in {
		    val probs = Seq(0.8, 0.8, 0.8).map(value => Probability(value))
		    
		    val model = Samplable.bernouliTrial(probs.toEmpiricalSeq)
		    
		    val result = (1 to 1000).map(_ => model.sample)
		    
		    result.count(_ == true) must beBetween(750, 850)
		  }
		}
		
		"have a (fair) coin object which can be tossed / sampled" in {
		  implicit val r = rf.newRandom
		  
		  val model = Samplable.coinToss
		  
		  val result = (1 to 100).map(_ => model.sample)
		  
		  result.count(_ == true) must beBetween(40, 60)
		}
		
		//Covariance and contravariance tests (for compilation only)
		object PlayingWithVariance{
			class Random2 extends Random {   // Should this be being used somewhere?
				def nextThingey() = 12
			}

			class T
			class S extends T
			val isT: T = new S
			
			val t = new Samplable[T]{
				def sample() = new T
			}
			val s = new Samplable[S]{
				def sample() = new S
			}
			
			val res1 = t.sample()
			val res2 = s.sample()
			val res3 = s.sample()
			
			val u: Samplable[T] = s
			val w: Samplable[S] = s
			
			class U
			val p: Samplable[U] = t.combine(s, (a:T, b:S) => new U)
		}
	}
	
	trait createInstance extends Scope {
		implicit val rand = rf.newRandom
		
		val instance = new Samplable[Int] {
			val it = List(0,1,2,3,4,5,6,7,8,9).iterator
			
			def sample(): Int = it.next()
		}
	}
	
	trait createTwoInstance extends Scope {
	  implicit val rand = rf.newRandom
	  
	  val instance1 = new Samplable[Int] {
		val it = List(1,1,1,1,1).iterator
		def sample(): Int = it.next()
	  }
		    
	  val instance2 = new Samplable[Int] {
		val it = List(5,6,7,8,9).iterator
		def sample(): Int = it.next()
	  }
	}
}
