package sampler.r

import sampler.data.Empirical
import java.nio.file.Paths
import sampler.io.CSVTableWriter
import sampler.data.Types.Column

class DensityWrapper {

  def apply[A](e: Empirical[A]) = {
    def expand(key: A, repeats: Int) = {
      (1 to repeats).map(a => key).toList
    }
    
//    TODO something is wrong here - output graphs look wrong!
    
    val probs = e.probabilities
    
    val min = probs.map(pair => pair._2.value).toList.min
    
    val transformed = probs.map(pair => pair._1 -> (pair._2.value / min).round.toInt)
    
    val remapped = transformed.flatMap(k => expand(k._1, k._2))
    
    val c1 = new Column(remapped.asInstanceOf[Seq[Double]].toSeq, "value")			// TODO address problem with type inference??
    
    val path = Paths.get("", "examples", "r", "density.csv")
    
    val writer  = new CSVTableWriter(path, true)
    
    writer.apply(c1)
    
    val rScript =
"""
require(ggplot2)
data <- read.csv("density.csv")
ggplot(data, aes(x=value)) + geom_density()      
"""
    
    ScriptRunner(rScript, path.getParent().resolve("script"))
  }
}

/*
 * Simple R script
require(ggplot2)
data <- read.csv("density.csv")
ggplot(data, aes(x=value)) + geom_density()
* 
 * 
 */
