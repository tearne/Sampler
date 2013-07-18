package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PartitionSpec extends Specification{
	"Partition" should {
		"throw exception is values don't sum to 1" in {
			val s = IndexedSeq(0.1, 0.2, 0.2, 0.4).map(Probability(_))	//sums to 0.9
			Partition(s) must throwAn[AssertionError]
		}
		
		"not accept an IndexedSeq with zero elements" in {
			val s = IndexedSeq()
					
			Partition(s) must throwAn[AssertionError]
		}
		
		"be of length four when successfully created" in {
			//TODO factor out common setup
			val s = IndexedSeq(0.1, 0.2, 0.3, 0.4).map(Probability(_))
			Partition(s).size mustEqual 4
		}
		
		"contain the exact values used as input" in {
			val s = IndexedSeq(0.1, 0.2, 0.3, 0.4).map(Probability(_))
			Partition(s).probabilities mustEqual s
		}
		
		"produce a correct probability map when requested" in {
			val s = IndexedSeq(0.1, 0.2, 0.3, 0.4).map(Probability(_))
			val p = Partition(s).probabilities
			
			(p(0) mustEqual Probability(0.1)) and
			(p(1) mustEqual Probability(0.2)) and
			(p(2) mustEqual Probability(0.3)) and
			(p(3) mustEqual Probability(0.4))
		}
		
		"accept a zero within the values" in {
		  val s = IndexedSeq(0.1, 0.2, 0.0, 0.3, 0.4).map(Probability(_))
		  
		  Partition(s).size mustEqual 5
		}
		
		"when created fromWeights" in {
		  
			"should have the correct length" in {
				val s = IndexedSeq(1.0, 2, 3, 4)
				val p = Partition.fromWeights(s)
						
				p.size mustEqual 4
			}
			
			"should produce correct probability map" in {
				val s = IndexedSeq(1.0, 2.0, 3.0, 4.0)
				val p = Partition.fromWeights(s).probabilities
						
				(p(0) mustEqual Probability(0.1)) and
				(p(1) mustEqual Probability(0.2)) and
				(p(2) mustEqual Probability(0.3)) and
				(p(3) mustEqual Probability(0.4))
			}
			
			"should allow a weight of zero" in {
			  val s = IndexedSeq(1.0, 2.0, 0.0, 3.0, 4.0)
			  
			  Partition.fromWeights(s).size mustEqual 5
			}
			
			"should produce an error message when weight < 0 given" in {
				val s = IndexedSeq(1.0, -2, 3, 4)
						
				Partition.fromWeights(s) must throwAn[AssertionError]
			}
		} 
		
	}
}