package sampler.r

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.data.Empirical._

@RunWith(classOf[JUnitRunner])
class DensityWrapperSpec extends Specification {

  "Density Wrapper should produce a script" in {
    "which can be run in R" in {
      val emp = IndexedSeq(1,2,2,3).toEmpiricalTable
      
      val wrapper = new DensityWrapper
      
      wrapper.apply(emp)
    }
  }
}