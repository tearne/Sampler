package sampler.spike.farrington_old


import java.nio.file.{Files,Paths}
import java.time.YearMonth
import scala.annotation.elidable.ASSERTION
import scala.collection.immutable.TreeMap
import scala.io.Source
import scala.language.{postfixOps,implicitConversions}
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.rosuda.REngine.Rserve.RConnection
import sampler.r.rserve.RServeHelper
import java.time._
import java.time.Year
import java.time.ZoneId
import java.time.temporal.ChronoUnit._
import java.time.temporal.ChronoField
import java.time.Duration.of
import java.time.Duration
import java.time.temporal.ChronoUnit
import java.time.temporal.TemporalUnit
import scala.collection.SortedMap
import org.json4s.JsonAST.JValue
import org.json4s.native.JsonMethods
import java.nio.charset.Charset
import java.nio.file.Path
import java.io.OutputStream
import sampler.r.process.ScriptRunner


object EDS_convergence extends App {
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 500
  
  // Number of months for which to simulate data:
  val nData = 462
  val endYear = 2014 
  
  // Choose "short" or "long" outbreaks
  // outbreakLength = "short"
  val outbreakLength = "long"
  
  // Choose log-Normal or epidemic curve outbreak
  // val outbreakShape = "logNormal"
  val outbreakShape = "epidemicCurve"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  val magnitude = 5
  
  // Identifiers for results files
  val csvName = "convergence.csv" // CSV file to store simulated data from Scala
  val scriptName = "convergence.r" // R script to import the CSV and plot the data
  val pdfName = "convergence.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "convergence")
  
  //=======================
  // Simulation and output
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  //RServeHelper.shutdown
  RServeHelper.ensureRunning()
  val rCon = new RConnection
  val PODpar = try {
    val result = (0 until nSimulations).par.map{i =>
      println(i)
      val data = GenerateData.run(
        nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
      val EDS_APHA = EDS.run(data, endBaseline, Farrington.APHA)
      val detected_APHA = EDS.detected(EDS_APHA, data.start, data.end)
      if (detected_APHA) 1 else 0
    }
    (0 until nSimulations).map{i => 
      println(i)
      (0 to i).par.map(j => result(j)).sum.toDouble / (i+1)
    }
    
//    def loop(i: Int, acc: Int): Double = {
//      
//      val data = GenerateData.run(
//        nData, endYear, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)      
//      val EDS_APHA = EDS.run(data, endBaseline, Farrington.APHA)
//      val detected_APHA = EDS.detected(EDS_APHA, data.start, data.end)
//      val isFlag = if (detected_APHA) 1 else 0
//      
//      val POD = (acc + isFlag).toDouble / i
//      println("Iteration = " + i)
//      println("POD = " + POD)
//      
//      writer.write(s"${i}, ${POD}")
//      writer.newLine
//      
//      if (i == nSimulations) acc
//      else loop(i+1, acc + isFlag)
//      
//    }    
//    loop(1, 0)

  }
  finally {
    rCon.close
    RServeHelper.shutdown
  }
  
  val POD = PODpar.toIndexedSeq
  
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  for (i <- 0 until POD.length) {
    writer.write(s"${POD(i).toString}")
    writer.newLine
  }
  writer.close
  
  // Write R script which imports and outputs table (html)
  val rScript = 
    s"""
      
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper

    data = read.csv("$csvName")
      
    pod = data[[1]]
      
    n = length(pod)
    
    plot(1:n, pod, type="l")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
  

}