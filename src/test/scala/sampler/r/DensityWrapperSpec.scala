package sampler.r

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import sampler.data.Empirical._

@RunWith(classOf[JUnitRunner])
class DensityWrapperSpec extends Specification {

  "Density Wrapper should produce a script" in {
//    "which can be run in R" in {
//      val emp = IndexedSeq(1,2,2,3).toEmpiricalTable
//      
//      val wrapper = new DensityWrapper
//      
//      wrapper.apply(emp)
//    }
    
    "test with more complicated empirical table" in {
      import java.nio.file.Paths
      import sampler.io.ChainReader

      val pathspec = Paths.get("", "examples", "ncpSampleSize", "data", "coda")
  
      val chains = ChainReader(pathspec.toString())
      
      val name = "PPosNCPFaecesNonCage[6]"
      
      val chain = chains(name)
      
      println(name)
      println("Min: " + chain.min)
      println("Average: " + chain.sum/chain.length)
      println("Max: " + chain.max)
      
      val emp = chain.toEmpiricalTable
      
      val wrapper = new DensityWrapper
      
      wrapper.apply(emp)
    }
  }
}