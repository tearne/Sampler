package sampler.spike.farrington_old

import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.Path
import sampler.r.process.ScriptRunner
import sampler.spike.farrington_old.Farrington.FarNew
import sampler.spike.farrington_old.Farrington.APHA
import sampler.spike.farrington_old.Farrington.Stl

/*
  =========
  NOTES:
  Script which simulates a number of simulated outbreak data sets
  with varying magnitude and assesses performance of the EDS.
     
  Uses default parameters to simulate baseline and outbreak data
  (Default is Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      12/03/2015
  Last edit: 12/03/2015
  
  ==========
  USER-DEFINED PARAMETERS:

  nSimulations            Number of sets of simulated data to run
  nData                   Length of each data set (number of months)
  endYear                 Last year for which to simulate data
  outbreakLength          Length of the outbreak ("short" or "long")
  endBaseline             End month of baseline period
  endPreOutbreak          End month of pre-outbreak period
  endOutbreak             End of outbreak period
  
  csvName                 Name of csv file to store histogram data
  scriptName              Name of R script for plotting histograms
  pdfName                 Name of pdf of plots
  resultsDir              Name of directory where results will be saved
  
  =========
  FUNCTIONS:
  
  successRate             Calculates success of detection (%)
  
  =========  
  OUTPUTS:
    
  
  
  
  */


object EDS_magnitude extends App{
  
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
  
//  val mode = APHA
//  val mode = FarNew
  val mode = Stl
       
  // Choose directory to place data and results
  val resultsDir = Paths.get("results", "compareMagnitude")
  
  //=======================
  // Simulate outbreak data and calculate measures
  
  val nk = magnitude.length

  RServeHelper.ensureRunning()
  val stats = (0 until nSimulations).par.map{i =>
  //val stats = (0 until nSimulations).map{i =>
    println(i)
    
    // Simulate two sets of baseline data and combine to create full set
    val data = (0 until nk).map{j => 
      GenerateData.run(nData, endYear,outbreakShape, outbreakLength,
          endPreOutbreak, endOutbreak, magnitude(j))
    }
    
    // Run EDS for each data set
    val EDS_result = (0 until nk).map(i => EDS.run(data(i), endBaseline, mode))
    
    // Probability of detection
    val flag = (0 until nk).map(
      i => EDS.detected(EDS_result(i), data(i).start, data(i).end) )
    
    // Probability of consecutive detection
    val consecutive = (0 until nk).map(
      i => EDS.detectedConsecutive(EDS_result(i), data(i).start, data(i).end) )

    // False Positive Rate
    val fpr = (0 until nk).map(
      i => EDS.falsePositiveRate(EDS_result(i), data(i).start, data(i).end) )
    
    // False Positive Rate (consecutive)
    val fprCon = (0 until nk).map(
      i => EDS.fprConsecutive(EDS_result(i), data(i).start, data(i).end) )
    
    // Positive predictive value
    val ppv = (0 until nk).map(
      i => EDS.positivePredictive(EDS_result(i), data(i).start, data(i).end) )
    
    // Positive predictive value
    val ppvCon = (0 until nk).map(
      i => EDS.ppvConsecutive(EDS_result(i), data(i).start, data(i).end) )
    
    // Time To Detection
    val times = (0 until nk).map(
      i => EDS.timeToDetection(EDS_result(i), data(i).start, data(i).end) )
    val ttd = (0 until nk).map(
        i => if (times(i).length == 0) -1 else times(i).head )
        
    // Time To Consecutive Detection
    val timesCon = (0 until nk).map(
      i => EDS.timeToConsecutiveDetection(EDS_result(i), data(i).start, data(i).end) )
    val ttcd = (0 until nk).map(
        i => if (timesCon(i).length == 0) -1 else timesCon(i).head )
    
    // Proportion of Outbreak Times Detected
    val potd = (0 until nk).map(
      i => EDS.proportionDetected(EDS_result(i), data(i).start, data(i).end) )
    
    MeasureData(flag, consecutive, fpr, fprCon, ppv, ppvCon, ttd, ttcd, potd)
    
  }  
  RServeHelper.shutdown
  
  //=======================
  // Extract measures
    
  // Probability of detection
  val POD =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).POD(i)).count(i => i==true).toDouble / nSimulations )
  
  // Probability of consecutive detection
  val POCD =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).POCD(i)).count(i => i==true).toDouble / nSimulations )
        
  // False positive rate
  val FPR =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).FPR(i)).sum.toDouble / nSimulations )
  
  // False positive rate (consecutive)
  val FPRCon =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).FPRCon(i)).sum.toDouble / nSimulations )
        
  // Positive predictive value 
  val PPV =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).PPV(i)).sum.toDouble / nSimulations )
        
  // Positive predictive value (consecutive)
  val PPVCon =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).PPVCon(i)).sum.toDouble / nSimulations )
        
  // False positive rate
  val TTD =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).TTD(i)).groupBy(w => w).mapValues(_.size).toList.sorted )
        
  // Proportion of outbreak times detected
  val POTD =
    (0 until nk).map(i => 
      (0 until nSimulations).map(j => 
        stats(j).POTD(i)).sum.toDouble / nSimulations )
        
  
  //=======================
  // Print relevant information to console
  
  println("Probability of any detection for full data = " + POD)
  
  println("Probability of consecutive detection for full data = " + POCD)
  
  println("False positive rate for full data = " + FPR)
  
  println("False positive rate (consecutive) for full data = " + FPRCon)
  
  println("Positive prediction for full data = " + PPV)
  
  println("Positive prediction (consecutive) for full data = " + PPVCon)
    
  println("Proportion of outbreak times detected for full data = " + POTD)
  
  //=======================
  // Output and plot
  
  plotTwo(resultsDir, "Probability of detection", "POD.csv", "POD.r", "POD.pdf", magnitude, POD, POCD)
  plotTwo(resultsDir, "False positive rate", "FPR.csv", "FPR.r", "FPR.pdf", magnitude, FPR, FPRCon)
  plotTwo(resultsDir, "Positive predictive value", "PPV.csv", "PPV.r", "PPV.pdf", magnitude, PPV, PPVCon)
  
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
    val rScript = 
      s"""
        
      data = read.csv("$csvName")
      x = data[[1]]
      y1 = data[[2]]
      y2 = data[[3]]
            
      cmin = min(y1, y2)
      cmax = max(y1, y2)
      
      pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
      plot(x, y1, type="l", col=2, lwd=5, ylim = c(cmin, cmax), xlab="Magnitude of outbreak", ylab="$measure", )
      lines(x, y2, col=3, lwd=5)
      legend("topright", xpd=TRUE, inset=c(0.2,-0.65), legend=c("any alert", "consecutive alerts"), fill=2:3)
      
      dev.off()
      """
      
    // Run the script in R and save the resulting PDF in the results directory
    ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
    
  }
      
  
  

}