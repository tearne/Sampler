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
		
		"return the correct integer values in the specificied order" in new createInstance {
			
			def append(previous: List[Int]): List[Int] = {
				if(previous.length == 10) previous
				else append(previous.:+(instance.sample(rand)))
			}
			val sampleList = append(List(instance.sample(rand)))
			
			val expectedList = List(0,1,2,3,4,5,6,7,8,9)
			
			sampleList mustEqual expectedList
		}
		
		"have a working sampleUntil method that should" in {

			"return the first half of the sequence until the value 5 is sampled" in new createInstance {
				val resultsSeq = instance.until(_.size == 5).sample
				
				val expectedSeq = IndexedSeq(0,1,2,3,4)
				
				(resultsSeq.size mustEqual 5) and
				(resultsSeq mustEqual expectedSeq)
			}
			
			"return a series of 2 length lists when sampling until an even number is reached" in new createInstance {
				val untilInstance = instance.until(_.last % 2 == 0)
				
				val seq1 = untilInstance.sample
				val seq2 = untilInstance.sample
				val seq3 = untilInstance.sample
				
				val expected1 = IndexedSeq(0)
				val expected2 = IndexedSeq(1,2)
				val expected3 = IndexedSeq(3,4)
				
				(seq1 mustEqual expected1) and
				(seq2 mustEqual expected2) and
				(seq3 mustEqual expected3)
			}
		}
		
		"have a working filter method that should" in {
			
			"return 3-9 when filtering for a value greater than 2" in new createInstance{
				val newSamplable = instance.filter(_ > 2)
				
				def append(previous: List[Int]): List[Int] = {
					if(previous.length == 7) previous
					else append(previous.:+(newSamplable.sample(rand)))
				}
				val sampleList = append(List(newSamplable.sample(rand)))
				
				val expectedList = List(3,4,5,6,7,8,9)
				
				sampleList mustEqual expectedList
			}
			
			"return 0, 2, 4, 6, 8 when you filter for even numbers" in new createInstance {
				val newSamplable = instance.filter(_ % 2 == 0)

				def append(previous: List[Int]): List[Int] = {
					if(previous.length == 5) previous
					else append(previous.:+(newSamplable.sample(rand)))
				}
				val sampleList = append(List(newSamplable.sample(rand)))
				
				val expectedList = List(0,2,4,6,8)
				
				sampleList mustEqual expectedList
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