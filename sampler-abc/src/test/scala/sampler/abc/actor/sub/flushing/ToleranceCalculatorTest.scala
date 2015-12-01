package sampler.abc.actor.sub.flushing

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar
import sampler.abc.core.ToleranceCalculator

class ToleranceCalculatorTest extends FreeSpec with Matchers with MockitoSugar {

  "Tolerance component should" - {
    
  	//TODO use mock statistics?
    val instance = new ToleranceCalculator with StatisticsImpl
    
//    Component with StatisticsComponent
//		  with LoggingAdapterComponent {
//      val statistics = Statistics
//      val toleranceCalculator = new ToleranceCalculator{}
//      val logg = mock[LoggingAdapter]
//    }
    
    val weighted1 = Weighted(Scored(1, Seq(0.05, 0.1, 0.15)), 1)	// Mean rep score = 0.1
    val weighted2 = Weighted(Scored(2, Seq(0.15, 0.2, 0.25)), 1)	// Mean rep score = 0.2
    val weighted3 = Weighted(Scored(3, Seq(0, 0, 0)), 1)	// Mean rep score = 0.0
    
    val currentTolerance = 0.15
    
    "Calculate the median mean score as 0.2 and return old tolerance" in {
      val weighedParameters = Seq(weighted2)
      
      val result = instance.apply(weighedParameters, currentTolerance)
      
      assert(result === currentTolerance)
    }
    
    "Calculate the median mean score as 0.1 and return" in {
      val weighedParameters = Seq(weighted1)
      
      val result = instance.apply(weighedParameters, currentTolerance)
      
      result should be(0.1 +- 1e-6)
    }
    
    "Median mean score from multiple weighed sequences" in {
      val weighedParameters = Seq(weighted1, weighted2, weighted2)
      
      val result = instance.apply(weighedParameters, currentTolerance)
      
      assert(result === currentTolerance)
    }
    
    "Calculate the median mean score as 0 and return old tolerance" in {
      val weighedParameters = Seq(weighted3)
      
      val result = instance.apply(weighedParameters, currentTolerance)
      
      assert(result === currentTolerance)
    }
  }
}