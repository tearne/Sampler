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

    val weighted1 = Weighted(scored1, 0.3)
    val weighted2 = Weighted(scored2, 0.4)
    val weighted3 = Weighted(scored3, 0.5)
  }

  "Weigher should" - {
    val tolerance = 10.05
    val prevPopulation: Generation[T] = mock[Population[T]]
    val irrelevant = 0

    "Use PartielWeightCalc to weigh each scored particle" in new Setup {
      val scoredParticles = ScoredParticles(
        Seq(scored1, scored2, scored3)
      )

      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenReturn(Some(0.3))
      when(calculator.particleWeight(scored2, tolerance, prevPopulation)).thenReturn(Some(0.4))
      when(calculator.particleWeight(scored3, tolerance, prevPopulation)).thenReturn(Some(0.5))

      val weighJob = new WeighJob(scoredParticles, prevPopulation, tolerance)

      val expectedWeightedParticles = Success(WeighedParticles(
    		  Seq(weighted1, weighted2, weighted3), 
    		  irrelevant
      ))
      assertResult(expectedWeightedParticles) {
        instance.apply(weighJob)
      }
    }

    "Particles that fail weighing are filtered out and counted" in new Setup { 
      /*
       * Can happen if
       *  - calculated weight is None (e.g. remote particle not supported by current generation)
       *  - none of the reps for a particle met require tolerance, leading to weight of zero
       */
      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenReturn(None)
      when(calculator.particleWeight(scored2, tolerance, prevPopulation)).thenReturn(Some(0.4))
      when(calculator.particleWeight(scored3, tolerance, prevPopulation)).thenReturn(Some(0.5))
      

      val oneRejected = 1
      val scoredParticles = ScoredParticles(Seq(scored1, scored2, scored3))
      val weighJob = new WeighJob(scoredParticles, prevPopulation, tolerance)

      val expected = Success(
        WeighedParticles(Seq(weighted2, weighted3), oneRejected)
      )

      assertResult(expected) {
        instance.apply(weighJob)
      }
    }
    
    "Catch exceptions from PWCalc in returned Try" in new Setup {
      val exception = new RuntimeException()
      when(calculator.particleWeight(scored1, tolerance, prevPopulation)).thenThrow(exception)
      when(calculator.particleWeight(scored2, tolerance, prevPopulation)).thenReturn(Some(0.4))
      when(calculator.particleWeight(scored3, tolerance, prevPopulation)).thenReturn(Some(0.5))
      
      val scoredParticles = ScoredParticles(Seq(scored1, scored2, scored3))
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