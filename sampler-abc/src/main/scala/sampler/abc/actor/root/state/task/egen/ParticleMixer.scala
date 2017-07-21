package sampler.abc.actor.root.state.task.egen

import sampler.abc.actor.message.ScoredParticles
import sampler.abc.{ABCConfig, Scored, Weighted}
import sampler.maths.Random

trait ParticleMixer {
  self : ParticleMixerHelper =>

  def apply[P](
    gen: EvolvingGeneration[P],
    config: ABCConfig
  )(
    implicit random: Random
  ): Option[ScoredParticles[P]] = {

    val mixPool = gen.mixingPool

    val mixingSize: Int = config.mixPayloadSize

    val oneOfEachParticle: Map[Scored[P], Int] =
      mixPool.map { case Weighted(scored, _) =>
        scored -> 1
      }.toMap

    if (oneOfEachParticle.size > mixingSize) {
      val res = drawMixParticles(mixingSize, oneOfEachParticle)

      Some(ScoredParticles(res))
    } else if (mixPool.size > 0) {
      Some(
        ScoredParticles(oneOfEachParticle.keys.toSeq)
      )
    } else None
  }
}

trait ParticleMixerHelper{
  def drawMixParticles[P](number: Int, bag: Map[Scored[P], Int])(implicit r: Random): Seq[Scored[P]]
}

trait ParticleMixerHelperImpl extends ParticleMixerHelper{
  import sampler._

  def drawMixParticles[P](number: Int, bag: Map[Scored[P], Int])(implicit r: Random): Seq[Scored[P]] = {
    bag.draw(number).drawnCounts.keys.toSeq
  }
}

object ParticleMixer extends ParticleMixer with ParticleMixerHelperImpl