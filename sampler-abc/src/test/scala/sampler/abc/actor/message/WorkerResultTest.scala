package sampler.abc.actor.message

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}
import sampler.abc.{Scored, UUID, Weighted}
import org.mockito.Mockito._

class WorkerResultTest extends FreeSpec with Matchers with MockitoSugar {

	"ScoredParticles messages should" - {
		val p1 = Scored(1, Seq(0.5))
		val p2 = Scored(2, Seq(0.5))
		val p3 = Scored(3, Seq(0.5))
		val p4 = Scored(4, Seq(0.5))
    
		val scoredParticles = ScoredParticles(Seq(p1, p2))
		
		"be able to add aditional particles from another case class to the sequence" in {
			val toAdd = ScoredParticles(Seq(p3, p4))
     
			val combined = scoredParticles.add(toAdd)
      
			assert(combined.seq.length === 4)
			assert(combined.seq.contains(p1))
			assert(combined.seq.contains(p2))
			assert(combined.seq.contains(p3))
			assert(combined.seq.contains(p4))
		}
	
		"be able to add a raw sequence of scored particles to the sequence" in {
		  val toAdd = Seq(p3, p4)
		  
		  val combined = scoredParticles.add(toAdd)
	      
			assert(combined.seq.length === 4)
			assert(combined.seq.contains(p1))
			assert(combined.seq.contains(p2))
			assert(combined.seq.contains(p3))
			assert(combined.seq.contains(p4))
		}
    
		"return the size of the sequence" in {
			val instance1 = ScoredParticles(Seq(p1, p2))
			val instance2 = ScoredParticles(Seq(p1, p2, p3, p4))
      
			assert(instance1.size === 2)
			assert(instance2.size === 4)
		}
    
		"be able to add from empty" in {
			val emptySeq = ScoredParticles(Seq.empty[Scored[Int]])
			val toAdd = ScoredParticles(Seq(p1))
      
			val combined = emptySeq.add(toAdd)
      
			assert(combined.seq.length === 1)
			assert(combined.seq.contains(p1))
		}
	}
  
	"WeighedParticles messages should" - {
		val p1 = Weighted(Scored(1, Seq(0.5)), 0.5)
		val p2 = Weighted(Scored(2, Seq(0.5)), 0.5)
		val p3 = Weighted(Scored(3, Seq(0.5)), 0.5)
		val p4 = Weighted(Scored(4, Seq(0.5)), 0.5)

		val uuidA = mock[UUID]; when(uuidA.generatingNodeId).thenReturn(99)  // Made on another node
		val pA = Weighted(Scored(5, Seq(0.5), Some(uuidA)), 0.5)
		val uuidB = mock[UUID]; when(uuidB.generatingNodeId).thenReturn(99)  // Made on another node
		val pB = Weighted(Scored(6, Seq(0.5), Some(uuidB)), 0.5)

    val acceptedOneLocal = 1
		val rejectedFiveLocal = 5
    val rejectedTwoLocal = 2

		val weighedParticles = WeighedParticles(Seq(p1, p2), rejectedFiveLocal)
    
		"add particles from another instance" in {
			val toAdd = WeighedParticles(Seq(p3, p4, pA), rejectedTwoLocal)
    	
			val combined = weighedParticles.add(toAdd)
    	
			assert(combined.seq.length === 5)
			assert(combined.seq.contains(p1))
			assert(combined.seq.contains(p2))
			assert(combined.seq.contains(p3))
			assert(combined.seq.contains(p4))
      assert(combined.seq.contains(pA))
			assert(combined.numLocalParticlesRejected === rejectedFiveLocal + rejectedTwoLocal)
		}
    
		"return the size of the sequence" in {
			val instance1 = WeighedParticles(Seq(p1, p2), rejectedFiveLocal)
			val instance2 = WeighedParticles(Seq(p1, p2, p3, p4), rejectedFiveLocal)

			assert(instance1.size === 2)
			assert(instance2.size === 4)
		}

		"calculate acceptance ratios" - {
			"simple example" in {
				val instance1 = WeighedParticles(Seq(p1, p2, pA), rejectedFiveLocal)
				val instance2 = WeighedParticles(Seq(p1, p2, p3, p4, pA, pB), rejectedTwoLocal)
        val instance3 = WeighedParticles(Seq(pA, pB), rejectedTwoLocal)

				assert(instance1.acceptanceRatio === 2.0 / (2 + rejectedFiveLocal))
				assert(instance2.acceptanceRatio === 4.0 / (4 + rejectedTwoLocal))
        assert(instance3.acceptanceRatio === 0.0 / (0 + rejectedTwoLocal))
			}

			"never give acceptance ratio of NaN" in {
				assertResult(0)(WeighedParticles(Seq.empty, 0).acceptanceRatio)
				assertResult(1)(WeighedParticles(Seq(p1,p2), 0).acceptanceRatio)
        assertResult(0)(WeighedParticles(Seq(pA,pB), 0).acceptanceRatio)
			}
		}
	}
}