package sampler.r

import sampler.data.Empirical
import java.nio.file.Paths
import sampler.io.CSVTableWriter
import sampler.data.Types.Column
import java.nio.file.Path
import sampler.data.EmpiricalSeq

class Grapher(path: Path) {

  
  
  def writeDensity(data: Map[String, EmpiricalSeq[Int]]) = {
    def expand(key: Int, repeats: Int) = {
      (1 to repeats).map(a => key).toList
    }
    
    val key = data.keys.head
    val value = data(key)
    
    val probs = value.probabilities
    
    val min = probs.map(pair => pair._2.value).toList.min
    
    val transformed = probs.map(pair => pair._1 -> (pair._2.value / min).round.toInt)
    
    val remapped = transformed.flatMap(k => expand(k._1, k._2))
    
    val c1 = new Column(remapped.toSeq, "value")
    
    val writer  = new CSVTableWriter(path.resolve(key + ".csv"), true)
    
    writer.apply(c1)
    
    val builder = new StringBuilder
    builder.append("require(ggplot2)\n")
    
    builder.append("data <- read.csv(\"" + key + ".csv\")\n")
    builder.append("ggplot(data, aes(x=value)) + geom_density()\n")
    
    val rScript = builder.toString
    
    ScriptRunner(rScript, path.resolve("script"))
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
