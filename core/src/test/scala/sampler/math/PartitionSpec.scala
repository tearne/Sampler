package sampler.math

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PartitionSpec extends Specification{
	"Partition" should {
		"throw exception is values don't sum to 1" in {
			val s = IndexedSeq(0.1, 0.2, 0.2, 0.4)	//sums to 0.9
			Partition(s) must throwAn[AssertionError]
		}
		
		"be of length four when successfully created" in {
			val s = IndexedSeq(0.1, 0.2, 0.3, 0.4)
			Partition(s).size mustEqual 4
		}
		
		"contain the exact widths used as input" in {
			val s = IndexedSeq(0.1, 0.2, 0.3, 0.4)
			Partition(s).widths mustEqual s
		}
		
		"produce a correct probability map when requested" in {
			val s = IndexedSeq(0.1, 0.2, 0.3, 0.4)
			val p = Partition(s).probabilities
			
			(p(0) mustEqual Probability(0.1)) and
			(p(1) mustEqual Probability(0.2)) and
			(p(2) mustEqual Probability(0.3)) and
			(p(3) mustEqual Probability(0.4))
		}
		
		"have the create length when created using fromWeights" in {
		  val s = Iterable(1.0, 2, 3, 4)
		  val p = Partition.fromWeights(s)
		  
		  p.size mustEqual 4
		}
		
		"produce correct probability map when created fromWeights" in {
		  
		  // TODO: change to indexed seq when Partition code changed
		  
		  val s = Iterable(1.0, 2, 3, 4)
		  val p = Partition.fromWeights(s).probabilities
		  
		  (p(0) mustEqual Probability(0.1)) and
		  (p(1) mustEqual Probability(0.2)) and
		  (p(2) mustEqual Probability(0.3)) and
		  (p(3) mustEqual Probability(0.4))
		}
		
		"Show an error message when weight < 0 given" in {
		  val s = Iterable(1.0, -2, 3, 4)
		  
		  Partition.fromWeights(s) must throwAn[IllegalArgumentException](message = "Weight must be strictly positive, found -2.0")
		}
	}
}