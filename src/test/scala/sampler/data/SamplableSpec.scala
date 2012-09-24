package sampler.data

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.Random
import org.specs2.specification.Scope

@RunWith(classOf[JUnitRunner])
class SamplableSpec extends Specification {
	"Samplable" should {
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
		
	}
	
	trait createInstance extends Scope {
		implicit val rand = new Random()
		
		val instance = new Samplable[Int] {
			val it = List(0,1,2,3,4,5,6,7,8,9).iterator
			
			def sample(implicit r: Random): Int = it.next()
		}
	}
}