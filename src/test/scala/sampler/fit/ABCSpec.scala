package sampler.fit

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ABCSpec extends Specification {

  "ABC parameters" should {
    "store and return all the correct information" in {
      val repetitions = 10
      val particles = 100
      val startTolerance = 1E4
      val refinementAttemps = 5
      val timeout = 500
      
      val parameters = new ABCParameters(repetitions, particles, startTolerance, refinementAttemps, timeout)
      
      (parameters.reps mustEqual repetitions) and
      (parameters.particles mustEqual particles) and
      (parameters.tolerance mustEqual startTolerance) and
      (parameters.refinements mustEqual refinementAttemps) and
      (parameters.timeout mustEqual timeout)
    }
  }
}