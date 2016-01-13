package sampler.abc.actor.sub.worker

import org.scalatest.FreeSpec
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers._
import java.util.concurrent.atomic.AtomicBoolean


class AborterTest extends FreeSpec with MockitoSugar  {  


  class testableAborter extends Aborter
  
    trait Setup {
    val instance = new Aborter {}
    val exception = new DetectedAbortionException()
  }
  
  "Aborter should /" - {
    "when checkIfAborted is called it has been aborted then expect DetectedAbortionException" in new Setup {
      instance.abort()
      intercept[DetectedAbortionException]{
        instance.checkIfAborted
      }
    }  
    "when checkIfAborted is called it has been reset then expect no exception " in new Setup {
      instance.reset()
      assertResult(){
        instance.checkIfAborted
      }
    }
  }
}