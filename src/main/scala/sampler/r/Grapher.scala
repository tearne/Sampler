package sampler.r

import sampler.data.Empirical
import java.nio.file.Paths
import sampler.io.CSVTableWriter
import sampler.data.Types.Column
import java.nio.file.Path
import sampler.data.EmpiricalSeq
import sampler.math.Probability

class Grapher(path: Path) {
  
  def writeIntDensity(data: Map[String, EmpiricalSeq[Int]]) = {
    def expand(key: Int, repeats: Int) = {
      (1 to repeats).map(a => key).toList
    }
    
    def transformToDist(map: Map[Int, Probability]) = {
      val min = map.map(pair => pair._2.value).toList.min
      val normalised = map.map(pair => pair._1 -> (pair._2.value / min).round.toInt)
      normalised.flatMap(k => expand(k._1, k._2)).toSeq
    }
    
    def writeDensity(name: String, values: Seq[Int]) = {
      val column = new Column(values, "value")
      val writer  = new CSVTableWriter(path.resolve(name + ".csv"), true)
    
      writer.apply(column)
    }
    
    def rScriptBuilder(plotNames: Seq[String]) = {
      val builder = new StringBuilder
      builder.append("require(ggplot2)\n")
      
      plotNames.foreach{a =>
      builder.append("data <- read.csv(\"" + a + ".csv\")\n")
      builder.append("ggplot(data, aes(x=value)) + geom_density()\n")
      }
      builder.toString
    }
    
    val names = data.keys.toSeq
    
    val probs = (names map(key => data(key).probabilities))
    
    val transformed = probs map (a => transformToDist(a))
    
    val nameMap = names zip transformed
    
    nameMap foreach(a => writeDensity(a._1, a._2))
        
    val builder = new StringBuilder
    builder.append("require(ggplot2)\n")
    
    val rScript = rScriptBuilder(names)
    
    ScriptRunner(rScript, path.resolve("scriptInt"))
  }
  
  def writeDoubleDensity(data: Map[String, EmpiricalSeq[Double]]) = {
    def expand(key: Double, repeats: Int) = {
      (1 to repeats).map(a => key).toList
    }
    
    def transformToDist(map: Map[Double, Probability]) = {
      val min = map.map(pair => pair._2.value).toList.min
      val normalised = map.map(pair => pair._1 -> (pair._2.value / min).round.toInt)
      normalised.flatMap(k => expand(k._1, k._2)).toSeq
    }
    
    def writeDensity(name: String, values: Seq[Double]) = {
      val column = new Column(values, "value")
      val writer  = new CSVTableWriter(path.resolve(name + ".csv"), true)
    
      writer.apply(column)
    }
    
    def rScriptBuilder(plotNames: Seq[String]) = {
      val builder = new StringBuilder
      builder.append("require(ggplot2)\n")
      
      plotNames.foreach{a =>
      builder.append("data <- read.csv(\"" + a + ".csv\")\n")
      builder.append("ggplot(data, aes(x=value)) + geom_density()\n")
      }
      builder.toString
    }
    
    val names = data.keys.toSeq
    
    val probs = (names map(key => data(key).probabilities))
    
    val transformed = probs map (a => transformToDist(a))
    
    val nameMap = names zip transformed
    
    nameMap foreach(a => writeDensity(a._1, a._2))
        
    val builder = new StringBuilder
    builder.append("require(ggplot2)\n")
    
    val rScript = rScriptBuilder(names)
    
    ScriptRunner(rScript, path.resolve("scriptDouble"))
  }
}
