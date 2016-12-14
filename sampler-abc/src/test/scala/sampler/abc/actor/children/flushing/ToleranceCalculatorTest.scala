package sampler.abc.actor.children.flushing

import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FreeSpec, Matchers}
import sampler.abc.{ABCConfig, Scored, Weighted}
import sampler.empirical.EmpiricalImplicits

class ToleranceCalculatorTest extends FreeSpec with Matchers with MockitoSugar {

  "Tolerance component should" - {
  	//TODO use mock statistics?
    val instance = new ToleranceCalculator with EmpiricalImplicits
    
    val weighted1 = Weighted(Scored(1, Seq(0.05, 0.1, 0.15)), 1)	// Mean rep score = 0.1
    val weighted2 = Weighted(Scored(2, Seq(0.15, 0.2, 0.25)), 1)	// Mean rep score = 0.2
    val weighted3 = Weighted(Scored(3, Seq(0, 0, 0)), 1)	        // Mean rep score = 0.0
    val weighted4 = Weighted(Scored(4, Seq(0.01, 0.02, 0.03, 0.04, 0.05)), 1) // Mean rep score = 0.03
    
    val currentTolerance = 0.15
    
    def getConfig(tolPrecentile: Double): ABCConfig = new ABCConfig(null){
		  override lazy val toleranceDescentPercentile = tolPrecentile
	  }
    
    "Use 1st percentile of mean rep scores" in {
      val weighedParameters = Seq(weighted1, weighted2, weighted4)
      
      val result = instance.apply(weighedParameters, getConfig(0.2), currentTolerance)
      
      result should be(0.03 +- 1e-6)
    }
    
    "Calculate the median mean score as 0.2 and so return old tolerance" in {
      val weighedParameters = Seq(weighted2)
      
      val result = instance.apply(weighedParameters, getConfig(0.5), currentTolerance)
      
      assert(result === currentTolerance)
    }
    
    "Calculate the median mean score as 0.1 and return" in {
      val weighedParameters = Seq(weighted1)
      
      val result = instance.apply(weighedParameters, getConfig(0.5), currentTolerance)
      
      result should be(0.1 +- 1e-6)
    }
    
    "Median mean score from multiple weighed sequences" in {
      val weighedParameters = Seq(weighted1, weighted2, weighted2)
      
      val result = instance.apply(weighedParameters, getConfig(0.5), currentTolerance)
      
      assert(result === currentTolerance)
    }
    
    "Calculate the median mean score as 0 and return old tolerance" in {
      val weighedParameters = Seq(weighted3)
      
      val result = instance.apply(weighedParameters, getConfig(0.5), currentTolerance)
      
      assert(result === currentTolerance)
    }
  }
}