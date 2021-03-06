package sampler.abc.actor.children.worker

import org.scalatest.FreeSpec
import org.scalatest.mockito.MockitoSugar

class AborterTest extends FreeSpec with MockitoSugar {  
  
  trait Setup {
    val instance = new Aborter {}
    val exception = new DetectedAbortionException()
  }
  
  "Aborter should /" - {
    "throw exceptions if and only if aborted" - {
    	"checkIfAborted throws DetectedAbortionException if it has been aborted" in new Setup {
    		instance.abort()
    		intercept[DetectedAbortionException]{
    			instance.checkIfAborted
    		}
    	}  
    	"checkIfAborted doesn't throw exception if not aborted" in new Setup {
    		instance.checkIfAborted
    	}      
    	"resetting" in {
    		fail("TODO")
    		// abort, reset, check no exception
    	}
    }
    "query abort status" - {
      "when not aborted" in fail("TODO")
      "when aborted" in fail("TODO")
      "when reset" in fail("TODO")
    }
  }
}