package sampler.abc.actor.main.component.helper

import sampler.math.Random
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.actor.main.EvolvingGeneration
import sampler.Implicits
import sampler.abc.ABCConfig
import sampler.abc.actor.main.Weighted

class ParticleMixer {
	
  def apply[P](
  		gen: EvolvingGeneration[P], 
  		config: ABCConfig
  	)(
  		implicit random: Random): Option[ScoredParticles[P]] = {
    val weightedParticles = gen.weighed
		
    val mixingSize: Int = config.mixPayloadSize
	
    if(weightedParticles.size > mixingSize) {
      val oneOfEachParticle = 
      	weightedParticles.seq.map{case Weighted(scored, _) =>
          	scored -> 1
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
      .map(_.scored)
			
      Some(ScoredParticles(res))
    } else None
  }
}