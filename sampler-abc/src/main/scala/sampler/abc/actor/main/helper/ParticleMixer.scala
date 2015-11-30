package sampler.abc.actor.main.helper

import sampler.abc.config.ABCConfig
import sampler.math.Random
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.Tagged
import sampler.abc.actor.main.EvolvingGeneration
import sampler.Implicits

class ParticleMixer {
	
  def apply[P](
  		gen: EvolvingGeneration[P], 
  		abcParameters: ABCConfig
  	)(
  		implicit random: Random): Option[ScoredParticles[P]] = {
    val weightedParticles = gen.weighed
		
    val mixingSize = abcParameters.cluster.mixPayloadSize
	
    if(weightedParticles.size > mixingSize) {
      val oneOfEachParticle = 
      	weightedParticles.seq.map{case Tagged(weighted, uid) =>
          	Tagged(weighted.scored, uid) -> 1
        	}
        	.toMap

      import sampler.Implicits.SamplableMap
			val res = oneOfEachParticle.draw(mixingSize).drawnCounts.map{
        case (scoredParticle, count) => scoredParticle
      }.toSeq		
			
      Some(ScoredParticles(res))
    } else if(weightedParticles.size > 0){
      val res = weightedParticles
      .seq
      .map{case Tagged(weighted, uid) =>
        Tagged(weighted.scored, uid)
      }
			
      Some(ScoredParticles(res))
    } else None
  }
}