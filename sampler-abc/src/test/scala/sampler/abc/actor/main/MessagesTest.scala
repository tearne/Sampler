package sampler.abc.actor.main

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import sampler.abc.Scored
import sampler.abc.Weighted

class MessagesTest extends FreeSpec with Matchers {

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
		val numRejected1 = 5
    val numRejected2 = 2
		
		val weighedParticles = WeighedParticles(Seq(p1, p2), numRejected1)
    
		"add particles from another instance" in {
			val toAdd = WeighedParticles(Seq(p3, p4), numRejected2)
    	
			val combined = weighedParticles.add(toAdd)
    	
			assert(combined.seq.length === 4)
			assert(combined.seq.contains(p1))
			assert(combined.seq.contains(p2))
			assert(combined.seq.contains(p3))
			assert(combined.seq.contains(p4))
			assert(combined.numRejected === numRejected1 + numRejected2)
		}
    
		"return the size of the sequence" in {
			val instance1 = WeighedParticles(Seq(p1, p2), numRejected1)
			val instance2 = WeighedParticles(Seq(p1, p2, p3, p4), numRejected1)

			assert(instance1.size === 2)
			assert(instance2.size === 4)
		}
		
		"calculate the acceptance rate" in {
		  val instance1 = WeighedParticles(Seq(p1, p2), numRejected1)
			val instance2 = WeighedParticles(Seq(p1, p2, p3, p4), numRejected2)

			assert(instance1.acceptanceRatio === 2.0 / (2 + numRejected1))
			assert(instance2.acceptanceRatio === 4.0 / (4 + numRejected2))
		}
		
		"not give acceptance ratio of NaN" in {
		  assertResult(0)(WeighedParticles(Seq.empty, 0).acceptanceRatio)
		  assertResult(1)(WeighedParticles(Seq(p1,p2), 0).acceptanceRatio)
		}
	}
}