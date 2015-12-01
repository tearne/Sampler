package sampler.abc.actor.sub.worker

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import sampler.abc.Model
import sampler.abc.Prior
import sampler.abc.Scored
import org.mockito.Mockito._
import org.mockito.Matchers._
import sampler.abc.Weighted
import sampler.abc.actor.sub.WeighJob
import sampler.abc.Generation
import sampler.abc.Population

class ParticleWeightCalculatorTest extends FreeSpec with MockitoSugar {
	type T = Int
	
	trait Setup {
		val model = mock[Model[T]]
		val prior = mock[Prior[T]]
		when(model.prior).thenReturn(prior)
		
		val aborter = new Aborter {}
		val instance = new ParticleWeightCalculator(model, aborter)
	}
	
	"Weigher should /" - {
		val tolerance = 0.5
		val scoredParticle = Scored(1, Seq(0.4, 0.49, 0.5, 0.51)) // 2/4 under threshold
		val prevPopulation: Generation[T] = Population(Map(1 -> 0.2, 2 -> 0.8), 0, 0)
		
		"when using a Population as the previous generation /" - {
			"return result, filtering out reps which don't meet tolerance" in new Setup {
				when(prior.density(anyInt)).thenReturn(0.5)
				when(model.perturbDensity(1, 1)).thenReturn(0.3)
				when(model.perturbDensity(2, 1)).thenReturn(0.1)

				val fHat = 2.0 / 4
				val numer = fHat * 0.5
				val denom = 0.2 * 0.3 + 0.8 * 0.1 
				
				assertResult(Some(numer / denom)){
					instance.particleWeight(
							scoredParticle, 
							tolerance,
							prevPopulation)
				}
			}
			"return None if fHat is zero" in new Setup {
				pending 
			}
			"return None if the particle is outside the prior" in new Setup {
				pending
			}
			"throw exception if no previous particle could have perturbed to this one" in new Setup {
				pending
			}
		}
		
		"When using the model prior as the previous (zero) generation /" - {
			"returns non zero 'fHat', filtering out reps which don't meet tolerance" in new Setup { 
				pending 
			}
			"returns zero if fHat is zero" in new Setup { 
				pending
			}
		}
	}
}