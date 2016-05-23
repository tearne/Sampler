//package sampler.abc.actor.sub.worker
//
//import org.scalatest.FreeSpec
//import org.scalatest.mock.MockitoSugar
//import sampler.abc.Model
//import sampler.abc.Prior
//import sampler.abc.Scored
//import org.mockito.Mockito._
//import org.mockito.Matchers._
//import sampler.abc.Weighted
//import sampler.abc.actor.sub.WeighJob
//import sampler.abc.Generation
//import sampler.abc.Population
//import sampler.abc.UseModelPrior
//
//class ParticleWeightCalculatorTest extends FreeSpec with MockitoSugar {
//  type T = Int //Pretend model parameters
//
//  trait Setup {
//    val model = mock[Model[T]]
//    val prior = mock[Prior[T]]
//    when(model.prior).thenReturn(prior)
//
//    val aborter = new Aborter {}
//    val instance = new ParticleWeightCalculator(model, aborter)
//  }
//
//  "Weigher should /" - {
//    val tolerance = 0.495
//    val scoredParticle = Scored(1, Seq(0.4, 0.49, 0.5, 0.51)) // 2/4 under threshold
//    val prevPopulation: Generation[T] = Population(Map(1 -> 0.2, 2 -> 0.8), 0, 0, 0.0)
//
//    "when using a Population as the previous generation /" - {
//      "return result, filtering out reps which don't meet tolerance" in new Setup {
//        when(prior.density(anyInt)).thenReturn(0.5)
//        when(model.perturbDensity(1, 1)).thenReturn(0.3)
//        when(model.perturbDensity(2, 1)).thenReturn(0.1)
//
//        val fHat = 2.0 / 4
//        val numer = fHat * 0.5
//        val denom = 0.2 * 0.3 + 0.8 * 0.1
//
//        assertResult(Some(numer / denom)) {
//          instance.particleWeight(
//            scoredParticle,
//            tolerance,
//            prevPopulation)
//        }
//      }
//      "return Some(zero) even if fHat is zero" in new Setup {
//        //Making tolerance zero results in an fHat of 0
//        val zeroTol = 0
//        //Keep the weight denominator non-zero
//        when(prior.density(anyInt)).thenReturn(0.5)
//        when(model.perturbDensity(anyInt, anyInt)).thenReturn(0.5)
//      	
//        assertResult(Some(0)) {
//          instance.particleWeight(
//            scoredParticle,
//            0,
//            prevPopulation)
//        }
//      }
//      "return None if the particle is outside the prior" in new Setup {
//      	//This scenario should only occur when particles are
//      	// received from non-local workers via mixing.
//      	
//        //If particle is outside prior it has a density of zero
//        when(prior.density(anyInt)).thenReturn(0)
//        assertResult(None) {
//          instance.particleWeight(
//            scoredParticle,
//            tolerance,
//            prevPopulation)
//        }
//      }
//    }
//
//    "When using the model prior as the previous (zero) generation /" - {
//      "returns non zero 'fHat', filtering out reps which don't meet tolerance" in new Setup {
//        val prevPopulation: Generation[T] = UseModelPrior(tolerance)
//
//        assertResult(Some(0.5)) {
//          instance.particleWeight(
//            scoredParticle,
//            tolerance,
//            prevPopulation)
//        }
//
//      }
//      "returns zero if fHat is zero" in new Setup {
//        val tolerance = 0
//        val prevPopulation: Generation[T] = UseModelPrior(tolerance)
//
//        assertResult(Some(0)) {
//          instance.particleWeight(
//            scoredParticle,
//            tolerance,
//            prevPopulation)
//        }
//      }
//    }
//  }
//}