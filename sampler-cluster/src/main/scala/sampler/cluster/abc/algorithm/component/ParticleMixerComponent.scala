package sampler.cluster.abc.algorithm.component

import sampler.math.StatisticsComponent
import sampler.cluster.abc.algorithm.Generation
import sampler.cluster.abc.config.ABCConfig
import sampler.cluster.abc.actor.ScoredParticles
import sampler.cluster.abc.actor.Tagged
import sampler.Implicits.SamplableMap
import sampler.math.Random

trait ParticleMixerComponent {
  this: StatisticsComponent =>
    
  val particleMixer: ParticleMixer
    
  trait ParticleMixer {
    implicit val r = Random
    
    def apply[P](gen: Generation[P], abcParameters: ABCConfig): Option[ScoredParticles[P]] = {
      val weightedParticles = gen.weighted
			
      val mixingSize = abcParameters.cluster.mixPayloadSize
		
      if(weightedParticles.size > mixingSize) {
        val oneOfEachParticle = 
          weightedParticles.seq
          .map{case Tagged(weighted, uid) =>
            Tagged(weighted.scored, uid) -> 1
        }
        .toMap

        val res = oneOfEachParticle.draw(mixingSize)._2.map{
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

}