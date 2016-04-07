package farrington.study

import farrington.core.simulate.SimulateOutbreakData
import org.rosuda.REngine.Rserve.RConnection
import sampler.r.rserve.RServeHelper
import java.nio.file.Paths
import farrington.core.algorithm.Farrington
import farrington.core.algorithm.EDS
import farrington.core.measures.Measures
import farrington.core.measures.AverageMeasures
import farrington.core.script.CreateRScript
import java.nio.charset.Charset
import java.nio.file.Path
import sampler.r.process.ScriptRunner
import java.nio.file.Files

object SensitivityToOutbreakMagnitude extends App {  
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 150
  
  // Number of months for which to simulate data:
  val nData = 462
  val endYear = 2014 
  
  // Choose "short" or "long" outbreaks
  //val outbreakLength = "short"
  val outbreakLength = "long"
  
  // Choose log-Normal or epidemic curve outbreak
  //val outbreakShape = "logNormal"
  val outbreakShape = "epidemicCurve"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  // Magnitude of outbreak
  val magnitude = (0.2 to 2 by 0.2)
  //val magnitude = (1 to 10)
  
  val mode = Farrington.APHA
//  val mode = Farrington.FarNew
//  val mode = Farrington.Stl
       
  // Choose directory to place data and results
  val resultsDir = Paths.get("results", "compareMagnitude")
  
  //=======================
  // Simulate outbreak data and calculate measures
  
  val nk = magnitude.length

  RServeHelper.ensureRunning()
  val stats = (0 until nSimulations).par.map{ i =>
  //val stats = (0 until nSimulations).map{i =>
    println(i)
    
    // Simulate two sets of baseline data and combine to create full set
    val data = (0 until nk).map{j => 
      SimulateOutbreakData.run(nData, endYear,outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude(j))
    }
    
    // Run EDS for each data set
    val EDS_result = (0 until nk).map(i => EDS.run(data(i), endBaseline, mode))
    
    // Calculate measures
    Measures.allMeasures(EDS_result(i), data(i).start, data(i).end)    
    
  }  
  RServeHelper.shutdown
  
  val avgMeasures = AverageMeasures.calculate(stats.toIndexedSeq, nSimulations)
  AverageMeasures.print(avgMeasures)
  
  //=======================
  // Output and plot
  
//  plotTwo(resultsDir, "Probability of detection", "POD.csv", "POD.r", "POD.pdf", magnitude, POD, POCD)
//  plotTwo(resultsDir, "False positive rate", "FPR.csv", "FPR.r", "FPR.pdf", magnitude, FPR, FPRCon)
//  plotTwo(resultsDir, "Positive predictive value", "PPV.csv", "PPV.r", "PPV.pdf", magnitude, PPV, PPVCon)
  
  def plotTwo(
      resultsDir: Path,
      measure: String,
      csvName: String,
      scriptName: String,
      pdfName: String,
      x: IndexedSeq[Any],
      data: IndexedSeq[Any],
      data2: IndexedSeq[Any]) = {
    
    // Create a directory to store results
    Files.createDirectories(resultsDir)
    
    // Write times to detection to CSV file for full data
    val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
    for (i <- 0 until data.length) {
      writer.write(s"${x(i).toString}, ${data(i).toString}, ${data2(i).toString}")
      writer.newLine
    }
    writer.close
    
    // Write R script which imports and plots data in a pdf
    val rScript = CreateRScript.plotTwo(measure, csvName, pdfName)
      
    // Run the script in R and save the resulting PDF in the results directory
    ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
    
  }

}