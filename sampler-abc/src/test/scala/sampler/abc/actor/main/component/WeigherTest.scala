package sampler.abc.actor.main.component

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import sampler.abc.Model
import sampler.abc.Prior
import sampler.abc.Scored
import org.mockito.Mockito._
import org.mockito.Matchers._
import sampler.abc.Weighted

class WeigherTest extends FreeSpec with MockitoSugar {
  "Gives weight option of " - {
		val model = mock[Model[Int]]
		val prior = mock[Prior[Int]]
		when(model.prior).thenReturn(prior)
	
		val singleScored = Scored(1, Seq(0.1))
		val prev = Map(1->0.5, 2->0.5)
		val tolerance = 0.5

		"None when both the denominator and numerator are 0" in {
		  when(prior.density(1)).thenReturn(0.0)
		  when(model.perturbDensity(anyInt, anyInt)).thenReturn(0.0)
		  
		  assertResult(None)(
		      instance.getWeightOption(model, singleScored, prev, tolerance)
		  )
    }
    
    "Result when positive numberator/denominators given" in {
      when(prior.density(1)).thenReturn(0.5)
		  when(model.perturbDensity(anyInt, anyInt)).thenReturn(1.0)
		  
		  val expectedWeighed = Weighted(singleScored, 0.5)
		  
		  assertResult(Some(expectedWeighed))(
		      instance.getWeightOption(model, singleScored, prev, tolerance)
		  )
    }
    
    "Filters reps that don't meet tolerance" in {
      val multipleScored = Scored(1, Seq(0.1, 0.9))
      when(prior.density(1)).thenReturn(0.5)
		  when(model.perturbDensity(anyInt, anyInt)).thenReturn(1.0)
		  
		  val expectedWeighed = Weighted(multipleScored, 0.25)
		  
		  assertResult(Some(expectedWeighed))(
		      instance.getWeightOption(model, multipleScored, prev, tolerance)
		  )
    }
  }
}