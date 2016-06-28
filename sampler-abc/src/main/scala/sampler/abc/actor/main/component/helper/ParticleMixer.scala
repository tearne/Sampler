package sampler.abc.actor.main.component.helper

import sampler._
import sampler.math.Random
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.EvolvingGeneration
import sampler.abc.ABCConfig
import sampler.abc.Weighted

class ParticleMixer {
	
  def apply[P](
  		gen: EvolvingGeneration[P], 
  		config: ABCConfig
  	)(
  		implicit random: Random): Option[ScoredParticles[P]] = {
    val mixPool = gen.mixingPool
		
    val mixingSize: Int = config.mixPayloadSize
	
    if(mixPool.size > mixingSize) {
      val oneOfEachParticle = 
      	mixPool.map{case Weighted(scored, _) =>
          	scored -> 1
        	}
        	.toMap

			val res = oneOfEachParticle.draw(mixingSize).drawnCounts.map{
        case (scoredParticle, count) => scoredParticle
      }.toSeq		
			
      Some(ScoredParticles(res))
    } else if(mixPool.size > 0){
      val res = mixPool
      .seq
      .map(_.scored)
			
      Some(ScoredParticles(res))
    } else None
  }
}