package sampler.abc.actor.root.state.task.egen

import sampler._
import sampler.abc.actor.message.ScoredParticles
import sampler.abc.{ABCConfig, Weighted}
import sampler.maths.Random

class ParticleMixer {

  def apply[P](
    gen: EvolvingGeneration[P],
    config: ABCConfig
  )(
    implicit random: Random
  ): Option[ScoredParticles[P]] = {

    val mixPool = gen.mixingPool

    val mixingSize: Int = config.mixPayloadSize

    if (mixPool.size > mixingSize) {
      //TODO after this, effective, consolidation it's...
      // possible that we have fewer particles and
      // then cane do the desired sample.
      //Prob only an issue for discrete params
      val oneOfEachParticle =
        mixPool.map { case Weighted(scored, _) =>
          scored -> 1
        }.toMap

      val res = oneOfEachParticle.draw(mixingSize).drawnCounts.keys.toSeq

      Some(ScoredParticles(res))
    } else if (mixPool.size > 0) {
      val res = mixPool
        .seq
        .map(_.scored)

      Some(ScoredParticles(res))
    } else None
  }
}