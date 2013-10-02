package sampler.r

import sampler.data.Empirical
import java.nio.file.Paths
import java.nio.file.Path
import sampler.data.EmpiricalSeq
import sampler.math.Probability
import sampler.data.EmpiricalTable
import sampler.io.CSVFile
import scala.language.implicitConversions

object QuickPlot {
  
  case class NamedDistribution[T: Fractional](dist: Seq[T], name: String)
  case class NamedDiscrete[T: Integral](dist: Seq[T], name: String)
  
  implicit class RichDistribution[T: Fractional](val dist: Seq[T]) {
    def continuousVariable(name: String) = NamedDistribution(dist, name)
  }
  
  implicit class RichDiscrete[T: Integral](val dist: Seq[T]) {
    def discreteVariable(name: String) = NamedDiscrete(dist, name)
  }
  
  implicit def Seq2NamedDistribution(dist: Seq[Double]) = NamedDistribution(dist, "na"+sampler.math.Random.nextInt(1000))
  implicit def Seq2NamedDiscrete(dist: Seq[Int]) = NamedDiscrete(dist, "")

  private def buildScript(fileName: String, lines: String*) = {
    val builder = new StringBuilder
    builder.append("require(ggplot2)\n")
	builder.append("require(reshape)\n")
	      
	builder.append("pdf(\"" + fileName + ".pdf\", width=8.27, height=5.83)\n")
	
	lines.foreach(builder.append(_))

	builder.append("dev.off()\n")
	      
	builder.toString
  }
  
  def writeDensity[T: Fractional](path: Path, fileName: String, data: NamedDistribution[T]*) = {
	val header = Seq("variable", "value")
    import Numeric.Implicits._
	
	def melted(data: Seq[NamedDistribution[T]]) = {
	  data.flatMap{case NamedDistribution(dist, name) => dist.map{name + "," + _.toDouble}}
	}
	
	CSVFile.write(path.resolve(fileName+".csv"), melted(data), false, true, header)
	  
	val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
	val line2 = "ggplot(data, aes(x=value, colour=variable)) + geom_density()\n"
	    
	val rScript = buildScript(fileName.toString, line1, line2)
	    
	ScriptRunner(rScript, path.resolve(fileName))
  }
  
  def writeDiscrete[T: Integral](path: Path, fileName: String, data: NamedDiscrete[T]*) = {
    val header = Seq("variable", "value")
    
    def melted(data: Seq[NamedDiscrete[T]]) = {
	  data.flatMap{case NamedDiscrete(dist, name) => dist.map{name + "," + _}}
	}
    
    CSVFile.write(path.resolve(fileName+".csv"), melted(data), false, true, header)

    val line1 = "data <- read.csv(\"" + fileName + ".csv\")\n"
    val line2 = "ggplot(data, aes(x=value, fill = variable)) + geom_bar()\n"

    val rScript = buildScript(fileName.toString, line1, line2)
	    
	ScriptRunner(rScript, path.resolve(fileName))
  }
  
  //TODO add an option for quickly plotting a single data set without bothering with Map
	
	//TODO
	//Consider making quickPlot able to plot from either
	// a) sequences (implemented above), and
	// b) EmpiricalTable/EmpiricalWeighted.
	//
	//Not sure if the latter is necessary.  It's
	//a bit more complicated, but AG had a method which 
	//seemed to work, based on the below
    
	/*
	def expand(key: Double, repeats: Int) = {
        (1 to repeats).map(a => key).toList
      }
      
    def transformToDist(map: Map[Double, Probability]) = {
        val min = map.map(pair => pair._2.value).toList.min
        val normalised = map.map(pair => pair._1 -> (pair._2.value / min).round.toInt)
        normalised.flatMap(k => expand(k._1, k._2)).toSeq
    }
    */
}

