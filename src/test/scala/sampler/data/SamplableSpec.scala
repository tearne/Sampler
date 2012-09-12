package sampler.data

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.math.Random
import org.specs2.specification.Scope

@RunWith(classOf[JUnitRunner])
class SamplableSpec extends Specification {
	
	"Samplable" should {
		
		"return the correct integer values in the specificied order" in new createInstance {
			
			(instance.sample(rand) mustEqual 0) and
			(instance.sample(rand) mustEqual 1) and
			(instance.sample(rand) mustEqual 2) and
			(instance.sample(rand) mustEqual 3) and
			(instance.sample(rand) mustEqual 4) and
			(instance.sample(rand) mustEqual 5) and
			(instance.sample(rand) mustEqual 6) and
			(instance.sample(rand) mustEqual 7) and
			(instance.sample(rand) mustEqual 8) and
			(instance.sample(rand) mustEqual 9)
		}
		
		"have a working sampleUntil method that should" in {

			"return the first half of the sequence until the value 5 is sampled" in new createInstance {
				
				val resultsSeq = instance.sampleUntil(_.size == 5)
				
				println(resultsSeq)
				
				(resultsSeq.size mustEqual 5) and
				(resultsSeq(0) mustEqual 0) and
				(resultsSeq(1) mustEqual 1) and
				(resultsSeq(2) mustEqual 2) and
				(resultsSeq(3) mustEqual 3) and
				(resultsSeq(4) mustEqual 4)
			}
			
			"return a series of 2 length lists when sampling until an even number is reached" in new createInstance {
				
				val seq1 = instance.sampleUntil(_.last % 2 == 0)
				val seq2 = instance.sampleUntil(_.last % 2 == 0)
				val seq3 = instance.sampleUntil(_.last % 2 == 0)
				
				(seq1.size mustEqual 1)
				(seq1.size mustEqual 1) and
				(seq2.size mustEqual 2) and
				(seq3.size mustEqual 2) and
				(seq1(0) mustEqual 0) and
				(seq2(0) mustEqual 1) and
				(seq2(1) mustEqual 2) and
				(seq3(0) mustEqual 3) and
				(seq3(1) mustEqual 4)
			}
		}
		
		"have a working filter method that should" in {
			
			"return 3-9 when filtering for a value greater than 2" in new createInstance{
				val newSamplable = instance.filter(_ > 2)
				
				(newSamplable.sample(rand) mustEqual 3) and
				(newSamplable.sample(rand) mustEqual 4) and
				(newSamplable.sample(rand) mustEqual 5) and
				(newSamplable.sample(rand) mustEqual 6) and
				(newSamplable.sample(rand) mustEqual 7) and
				(newSamplable.sample(rand) mustEqual 8) and
				(newSamplable.sample(rand) mustEqual 9)
			}
			
			"return 0, 2, 4, 6, 8 when you filter for even numbers" in new createInstance {
				val newSamplable = instance.filter(_ % 2 == 0)

				(newSamplable.sample(rand) mustEqual 0) and
				(newSamplable.sample(rand) mustEqual 2) and
				(newSamplable.sample(rand) mustEqual 4) and
				(newSamplable.sample(rand) mustEqual 6) and
				(newSamplable.sample(rand) mustEqual 8)
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