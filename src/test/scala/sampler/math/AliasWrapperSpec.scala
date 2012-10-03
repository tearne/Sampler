package sampler.math
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mock.Mockito

@RunWith(classOf[JUnitRunner])
class AliasWrapperSpec extends Specification with Mockito{

  "AliasWrapper" should {
    
    "throw an exception" in {
      
	    "if the probabilities don't add to one" in {
	      val random = mock[Random]
	      val probabilities = Vector(0.1, 0.2, 0.3, 0.3)
	      
	      new AliasWrapper(probabilities) must throwA[ProbabilityException]
	    }
	}
    
    "not throw an exception" in {
      
	    "if the probability sum is not equal to one because of a rounding error" in {
	      val random = mock[Random]
	      val seventh = 1.0/7.0
	      val forteenth = 1.0/14.0
	      
	      val probabilities = Vector(seventh, seventh, seventh, seventh, seventh, seventh, forteenth, forteenth)
	      // Sum = 0.9999999999999998
	    	  
	    }
    }
  
    "return random values if supplied with a real random" in {
      val probabilities = Vector(0.1, 0.2, 0.3, 0.4)
      
      val aliasWrapper = new AliasWrapper(probabilities)
      
      var listOfSamples: List[Int] = List()
      
      for(i <- 0 until 1000)
        listOfSamples = listOfSamples.+:(aliasWrapper.sample())
      
      (listOfSamples.count(_ == 0) must beBetween(50, 150)) and
      (listOfSamples.count(_ == 1) must beBetween(150, 250)) and
      (listOfSamples.count(_ == 2) must beBetween(250, 350)) and
      (listOfSamples.count(_ == 3) must beBetween(350, 450)) 
    }
  }
}