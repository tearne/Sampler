package sampler.abc

import org.scalatest.FreeSpec
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._

class ScoredTest extends FreeSpec with MockitoSugar {
  "Scored should" - {
    "Make an ID for you if using the apply buider" in fail("TODO")
  	"Throw exception if any of the scores are negative" in fail("TODO")
  	"Return number of reps as a double" in fail("TODO")
  	"Calculate the mean score" in fail("TODO")

    "Know if it was generated locally" in {
      val instanceLocal1 = Scored(null, null)
      val instanceLocal2 = Scored(null, null)

      val instanceNonLocal = Scored(null, null, None)

      val mockUUID = mock[UUID]
      when(mockUUID.generatingNodeId).thenReturn(99)
      val instanceRemote = Scored(null, null, Some(mockUUID))

      assert(instanceLocal1.wasLocallyGenerated)
      assert(instanceLocal2.wasLocallyGenerated)

      assert(!instanceNonLocal.wasLocallyGenerated)
      assert(!instanceRemote.wasLocallyGenerated)
    }
  }
}