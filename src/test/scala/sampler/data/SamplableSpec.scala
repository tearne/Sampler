package sampler.data

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.Random
import org.specs2.specification.Scope

@RunWith(classOf[JUnitRunner])
class SamplableSpec extends Specification {
	"Samplable" should {
		"have map" in todo
		"have flatMap" in todo
		"have combine" in todo
		"have convolution (+)" in todo
		"have cross correlation (-)" in todo
		
		"sample mocked values in order" in new createInstance {
			
			def append(previous: List[Int]): List[Int] = {
				if(previous.length == 10) previous
				else append(previous.:+(instance.sample(rand)))
			}
			val sampleList = append(List(instance.sample(rand)))
			
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
		
		"have filter method" in {
			"always sample greater than 2" in new createInstance{
				val newSamplable = instance.filter(_ > 2)
				
				def append(previous: List[Int]): List[Int] = {
					if(previous.length == 7) previous
					else append(previous.:+(newSamplable.sample(rand)))
				}
				val sampleList = append(List(newSamplable.sample(rand)))
				
				sampleList mustEqual List(3,4,5,6,7,8,9)
			}
			
			"always samepl even numbers" in new createInstance {
				val newSamplable = instance.filter(_ % 2 == 0)

				def append(previous: List[Int]): List[Int] = {
					if(previous.length == 5) previous
					else append(previous.:+(newSamplable.sample(rand)))
				}
				val sampleList = append(List(newSamplable.sample(rand)))
				
				sampleList mustEqual List(0,2,4,6,8)
			}
		}
		
		"have a Bernoilli trial object" in {
		  
		  import sampler.data.Empirical._
		  import sampler.math.Probability

		  implicit val r = new Random()
		  
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
		
		//TODO map, flatmap, combine, convolve, crossCorrelate
		
		//Covariance and contravariance tests (for compilation only)
		object PlayingWithVariance{
			class Random2 extends Random {
				def nextThingey() = 12
			}
			
			class T
			class S extends T
			val isT: T = new S
			
			val t = new Samplable[T, Random2]{
				def sample(implicit r: Random2) = new T
			}
			val s = new Samplable[S, Random]{
				def sample(implicit r: Random) = new S
			}
			
			val res1 = t.sample(new Random2)
			val res2 = s.sample(new Random)
			val res3 = s.sample(new Random2)
			
			val u: Samplable[T,Random] = s
			val w: Samplable[S,Random2] = s
			
			class U
			val p: Samplable[U,Random2] = t.combine(s, (a:T, b:S) => new U)
		}
	}
	
	trait createInstance extends Scope {
		implicit val rand = new Random()
		
		val instance = new Samplable[Int, Random] {
			val it = List(0,1,2,3,4,5,6,7,8,9).iterator
			
			def sample(implicit r: Random): Int = it.next()
		}
	}
}