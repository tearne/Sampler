package sampler.math

import org.scalatest.Matchers
import org.scalatest.FreeSpec
import org.scalatest.BeforeAndAfter

class RandomTest extends FreeSpec with BeforeAndAfter with Matchers{

  var random: Random = _

  before {
    random = Random
  }
  
  "generatesIntsInCorrectRange" in {
    val draw1 = random.nextInt(5)
    
    draw1 should be(2 +- 2)
  }
  
  "drawIntValuesAtRandom" in {
    def sample(samples: List[Int], currentIt: Int, numIts: Int): List[Int] = {
      if(currentIt>=numIts) samples
      else {
	    sample(samples.:+(random.nextInt(4)), currentIt+1, numIts)
	  }
    }
		    
    val requiredIterations = 1000
    val sampledInts = sample(List(), 0, requiredIterations)

    sampledInts.count(_ == 0) should be(250 +- 50)
    sampledInts.count(_ == 1) should be(250 +- 50)
    sampledInts.count(_ == 2) should be(250 +- 50)
    sampledInts.count(_ == 3) should be(250 +- 50)
  }
  
  "generatesDoublesInCorrectRange" in {
    val draw1 = random.nextDouble(0.5, 2.5)
    val draw2 = random.nextDouble(1.5, 2.5)
	val draw3 = random.nextDouble(3.0, 5.0)
	
	draw1 should be(1.5 +- 1.0)
    draw2 should be(2.0 +- 0.5)
    draw3 should be(4.0 +- 1.0)
  }
  
  "generatesBoolenasInGivenProportion" in {
    def booleanSample(samples: List[Boolean], p: Double, currentIt: Int, numIts: Int): List[Boolean] = {
	  if(currentIt >= numIts) samples
      else {
		booleanSample(samples.:+(random.nextBoolean(p)), p, currentIt+1, numIts)
	  }
    }
		    
	val probability = 0.9
	val requiredIterations = 1000
		    
	val sampledBooleans = booleanSample(List(), probability, 0, requiredIterations)
	
	sampledBooleans.count(_ == true) should be(900 +- 50)
    sampledBooleans.count(_ == false) should be(100 +- 50)
  }
  
  "exceptionWhenInvalidProbabilityToNextBoolean" in {
    intercept[RangeException[Double]] {
      random.nextBoolean(1.5)
    }

    intercept[RangeException[Double]] {
    	random.nextBoolean(-0.5)
    }
  }
}