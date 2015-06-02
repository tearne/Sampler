package sampler.abc.actor

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import sampler.abc.Scored
import sampler.abc.Weighted

class MessagesTest extends FreeSpec with Matchers {

	"ScoredParticles classes should" - {
		val p1 = Tagged(Scored(1, Seq(0.5)), 111111)
		val p2 = Tagged(Scored(2, Seq(0.5)), 111112)
		val p3 = Tagged(Scored(3, Seq(0.5)), 111113)
		val p4 = Tagged(Scored(4, Seq(0.5)), 111114)
    
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
    
		"be initialisable when empty" in {
			val emptySeq = ScoredParticles(Seq.empty[Tagged[Scored[Int]]])
			val toAdd = ScoredParticles(Seq(p1))
      
			val combined = emptySeq.add(toAdd)
      
			assert(combined.seq.length === 1)
			assert(combined.seq.contains(p1))
		}
	}
  
	"WeighedParticles classes should" - {
		val p1 = Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111)
		val p2 = Tagged(Weighted(Scored(2, Seq(0.5)), 0.5), 111112)
		val p3 = Tagged(Weighted(Scored(3, Seq(0.5)), 0.5), 111113)
		val p4 = Tagged(Weighted(Scored(4, Seq(0.5)), 0.5), 111114)
    
		val weighedParticles = WeighedParticles(Seq(p1, p2))
    
		"be able to add particles from another case class to the sequence" in {
			val toAdd = WeighedParticles(Seq(p3, p4))
    	
			val combined = weighedParticles.add(toAdd)
    	
			assert(combined.seq.length === 4)
			assert(combined.seq.contains(p1))
			assert(combined.seq.contains(p2))
			assert(combined.seq.contains(p3))
			assert(combined.seq.contains(p4))
		}
    
		"be able to add a raw sequence of weighed particles to the sequence" in {
			val toAdd = Seq(p3, p4)
			
			val combined = weighedParticles.add(toAdd)
			
			assert(combined.seq.length === 4)
			assert(combined.seq.contains(p1))
			assert(combined.seq.contains(p2))
			assert(combined.seq.contains(p3))
			assert(combined.seq.contains(p4))
		}
    
		"return the size of the sequence" in {
			val instance1 = WeighedParticles(Seq(p1, p2))
			val instance2 = WeighedParticles(Seq(p1, p2, p3, p4))

			assert(instance1.size === 2)
			assert(instance2.size === 4)
		}
	}
}