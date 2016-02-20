package sampler.abc.actor.sub.worker

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import sampler.abc.Scored
import sampler.abc.Generation
import sampler.abc.Population
import sampler.abc.Model
import scala.util.Try
import sampler.abc.actor.main.WeighedParticles
import sampler.abc.actor.sub.WeighJob
import sampler.abc.actor.main.Tagged
import sampler.abc.actor.main.ScoredParticles
import sampler.abc.Weighted
import org.mockito.Mockito._
import org.mockito.Matchers._
import scala.util.Success
import scala.util.Failure

class WeigherTest extends FreeSpec with MockitoSugar {
  type T = Int //Pretend model parameters

  trait Setup {
    val calculator = mock[ParticleWeightCalculator[T]]
    val instance = new Weigher(calculator)

    val scored1 = mock[Scored[T]]
    val scored2 = mock[Scored[T]]
    val scored3 = mock[Scored[T]]

    val taggedScoredParticle1 = Tagged(scored1, 1)
    val taggedScoredParticle2 = Tagged(scored2, 2)
    val taggedScoredParticle3 = Tagged(scored3, 3)

    val weightedParticle1 = Tagged(Weighted(scored1, 0.3), 1)
    val weightedParticle2 = Tagged(Weighted(scored2, 0.4), 2)
    val weightedParticle3 = Tagged(Weighted(scored3, 0.5), 3)
  }

  "Weigher should" - {
    val tolerance = 10.05
    val prevPopulation: Generation[T] = Population(Map(1 -> 0.2, 2 -> 0.8), 0, 0)
    val irrelevant = 0

    "Use PartielWeightCalc to weigh each scored particle" in new Setup {

      val scoredTaggedParticles = List(taggedScoredParticle1, taggedScoredParticle2, taggedScoredParticle3).toSeq
      val scoredParticles = ScoredParticles(scoredTaggedParticles)

      val weightedParticleSeq = List(weightedParticle1, weightedParticle2, weightedParticle3).toSeq
      val weightedParticles = Success(WeighedParticles(weightedParticleSeq, irrelevant))

      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenReturn(Some(0.3))
      when(calculator.particleWeight(scored2, tolerance, prevPopulation)).thenReturn(Some(0.4))
      when(calculator.particleWeight(scored3, tolerance, prevPopulation)).thenReturn(Some(0.5))

      val weighJob = new WeighJob(scoredParticles, prevPopulation, tolerance)

      assertResult(weightedParticles) {
        instance.apply(weighJob)
      }
    }

    "Preserve the tag present within each scored particle" in new Setup {
      val taggedScoredParticle = Tagged(scored1, 1)
      val scoredParticles = ScoredParticles(List(taggedScoredParticle).toSeq)
      val weightedParticles = Success(WeighedParticles(
          List(Tagged(Weighted(scored1, 0.3), 1)).toSeq, 
          irrelevant
      ))

      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenReturn(Some(0.3))

      val weighJob = new WeighJob(scoredParticles, prevPopulation, tolerance)

      val result = instance.apply(weighJob).get.seq

      val wp = result.seq.foreach { wp => assert(wp.id == taggedScoredParticle.id) }

    }

    "Particles that fail weighing are filtered out and counted" in new Setup { 
      /*
       * Can happen if
       *  - calculated weight is None (e.g. remote particle not supported by current generation)
       *  - none of the reps for a particle met require tolerance, leading to weight of zero
       */
      //Can happen if denominator is 0 which gives None in calculator
      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenReturn(None)
      when(calculator.particleWeight(scored2, tolerance, prevPopulation)).thenReturn(Some(0.4))
      when(calculator.particleWeight(scored3, tolerance, prevPopulation)).thenReturn(Some(0.5))
      val scoredTaggedParticles = List(taggedScoredParticle1, taggedScoredParticle2, taggedScoredParticle3).toSeq
      val scoredParticles = ScoredParticles(scoredTaggedParticles)
      val weightedParticleSeq = List(weightedParticle2, weightedParticle3).toSeq
      val oneRejected = 1
      val expected = Success(WeighedParticles(weightedParticleSeq, oneRejected))
      
      val weighJob = new WeighJob(scoredParticles, prevPopulation, tolerance)

      assertResult(expected) {
        instance.apply(weighJob)
      }
    }
    "Catch exceptions from PWCalc in returned Try" in new Setup {
      val exception = new RuntimeException()
      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenThrow(exception)
      when(calculator.particleWeight(scored2, tolerance, prevPopulation)).thenReturn(Some(0.4))
      when(calculator.particleWeight(scored3, tolerance, prevPopulation)).thenReturn(Some(0.5))
      val scoredTaggedParticles = List(taggedScoredParticle1, taggedScoredParticle2, taggedScoredParticle3).toSeq
      val scoredParticles = ScoredParticles(scoredTaggedParticles)
      val weightedParticleSeq = List(weightedParticle2, weightedParticle3).toSeq
      val weightedParticles = Failure(exception)
      
      val weighJob = new WeighJob(scoredParticles, prevPopulation, tolerance)

      assertResult(weightedParticles) {
        instance.apply(weighJob)
      }
      
    }
    
    "Not return anything that gets a weight of zero" in fail("TODO")
    "Not return anything that fails weighing" in fail("TODO")
  }
}