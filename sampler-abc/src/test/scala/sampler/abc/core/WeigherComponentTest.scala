package sampler.abc.algorithm.component

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import sampler.math.StatisticsComponent
import sampler.math.Statistics
import org.scalatest.mock.MockitoSugar
import sampler.abc.Model
import sampler.abc.Scored
import sampler.abc.Prior
import org.mockito.Mockito._
import org.mockito.Matchers.anyInt
import sampler.abc.Weighted

class WeigherComponentTest extends FreeSpec with Matchers with MockitoSugar {

  val instanceComponent = new WeigherComponent with StatisticsComponent{
    val statistics = Statistics
    val weigher = new Weigher{}
  }
  
  val instance = instanceComponent.weigher
  
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
  
  "Gives a consolidated weights table" in {
    val population = Seq(
        Weighted(Scored(1, Seq(0.1)), 0.1),
        Weighted(Scored(1, Seq(0.2)), 0.2),
        Weighted(Scored(2, Seq(0.3)), 0.3),
        Weighted(Scored(2, Seq(0.4)), 0.4)
    )
    
    val consolidatedMap = instance.consolidateToWeightsTable(population)
    
    val tolerance = 1e-6
    
    consolidatedMap.getOrElse(1, 0.0) should be(0.3 +- tolerance)
    consolidatedMap.getOrElse(2, 0.0) should be(0.7 +- tolerance)
  }
}