package sampler.r

import sampler.data.Empirical
import java.nio.file.Paths
import sampler.io.CSVTableWriter
import sampler.data.Types.Column

class DensityWrapper {

  def apply[A](e: Empirical[A]) = {
    val probs = e.probabilities
    
    val keys = probs.keySet.toSeq
    val values = keys.map(key => probs(key).value)
    
    val c1 = new Column(keys.map(key => key.toString), "Keys")			// TODO address problem with type inference??
    val c2 = new Column(values, "Values")
    
    val path = Paths.get("", "examples", "r", "density.csv")
    
    val writer  = new CSVTableWriter(path, true)
    
    // TODO need to reformat values (flatmap?) into structure amenable to density plotting in R
    
    writer.apply(c1, c2)
  }
}