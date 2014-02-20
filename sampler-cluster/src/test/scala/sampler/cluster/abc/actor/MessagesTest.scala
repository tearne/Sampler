package sampler.cluster.abc.actor

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import sampler.cluster.abc.Scored
import sampler.cluster.abc.Weighted

class MessagesTest extends FreeSpec with Matchers {

  "ScoredParticles classes should" - {
	val p1 = Tagged(Scored(1, Seq(0.5)), 111111)
	val p2 = Tagged(Scored(2, Seq(0.5)), 111112)
	val p3 = Tagged(Scored(3, Seq(0.5)), 111113)
	val p4 = Tagged(Scored(4, Seq(0.5)), 111114)
    
	val scoredParticles = ScoredParticles(Seq(p1, p2))
		
	"be able to add aditional particles to the sequence" in {
      val toAdd = ScoredParticles(Seq(p3, p4))
      
      val combined = scoredParticles.add(toAdd)
      
      assert(combined.seq.length === 4)
      assert(combined.seq.contains(p1))
      assert(combined.seq.contains(p2))
      assert(combined.seq.contains(p3))
      assert(combined.seq.contains(p4))
    }
    
    "empty the sequence of scored particles" in {
      val emptied = scoredParticles.empty
      assert(emptied.seq.isEmpty)
    }
  }
  
  "WeighedParticles classes should" - {
    val p1 = Tagged(Weighted(Scored(1, Seq(0.5)), 0.5), 111111)
    val p2 = Tagged(Weighted(Scored(2, Seq(0.5)), 0.5), 111112)
    val p3 = Tagged(Weighted(Scored(3, Seq(0.5)), 0.5), 111113)
    val p4 = Tagged(Weighted(Scored(4, Seq(0.5)), 0.5), 111114)
    
    val weighedParticles = WeighedParticles(Seq(p1, p2))
    
    "be able to add particles to the sequence" in {
      val toAdd = WeighedParticles(Seq(p3, p4))
    	
      val combined = weighedParticles.add(toAdd)
    	
      assert(combined.seq.length === 4)
      assert(combined.seq.contains(p1))
      assert(combined.seq.contains(p2))
      assert(combined.seq.contains(p3))
      assert(combined.seq.contains(p4))
    }
    
    "empty the sequence of weighed particles" in {
      val emptied = weighedParticles.empty
      assert(emptied.seq.isEmpty)
    }
  }
}