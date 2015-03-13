package sampler.spike.farrington

import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.Path
import sampler.r.process.ScriptRunner

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
  val nSimulations = 24
  
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
  val magnitude = (1 to 10)
      
  // Identifiers for results files
  val csv_full = "hiddenOutbreak_full.csv" // CSV file to store simulated data from Scala
  val csv_split1 = "hiddenOutbreak_split1.csv"
  val csv_split2 = "hiddenOutbreak_split2.csv"
  val scriptName = "plotHiddenOutbreak.r" // R script to import the CSV and plot the data
  val pdfName = "hiddenOutbreak.pdf" // PDF containing the plots
  
  // Choose directory to place data and results
  val resultsDir = Paths.get("results", "hiddenOutbreak")
  
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
    val EDS_result = (0 until nk).map(i => EDS_TS.run(data(i), endBaseline))
    
    // Probability of detection
    val flag = (0 until nk).map(
      i => EDS_TS.detected(EDS_result(i), data(i).start, data(i).end) )
    
    // Probability of consecutive detection
    val consecutive = (0 until nk).map(
      i => EDS_TS.detectedConsecutive(EDS_result(i), data(i).start, data(i).end) )

    // False Positive Rate
    val fpr = (0 until nk).map(
      i => EDS_TS.falsePositiveRate(EDS_result(i), data(i).start, data(i).end) )
    
    // False Positive Rate (consecutive)
    val fprCon = (0 until nk).map(
      i => EDS_TS.fprConsecutive(EDS_result(i), data(i).start, data(i).end) )
    
    // Positive predictive value
    val ppv = (0 until nk).map(
      i => EDS_TS.positivePredictive(EDS_result(i), data(i).start, data(i).end) )
    
    // Positive predictive value
    val ppvCon = (0 until nk).map(
      i => EDS_TS.ppvConsecutive(EDS_result(i), data(i).start, data(i).end) )
    
    // Time To Detection
    val times = (0 until nk).map(
      i => EDS_TS.timeToDetection(EDS_result(i), data(i).start, data(i).end) )
    val ttd = (0 until nk).map(
        i => if (times(i).length == 0) -1 else times(i).head )
    
    // Proportion of Outbreak Times Detected
    val potd = (0 until nk).map(
      i => EDS_TS.proportionDetected(EDS_result(i), data(i).start, data(i).end) )
    
    MeasureData(flag, consecutive, fpr, fprCon, ppv, ppvCon, ttd, potd)
    
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
  
  plotTwo(resultsDir, "POD.csv", "POD.pdf", POD, POCD)
  plotTwo(resultsDir, "FPR.csv", "FPR.pdf", FPR, FPRCon)
  plotTwo(resultsDir, "PPV.csv", "PPV.pdf", PPV, PPVCon)
  
  def plotTwo(
      resultsDir: Path,
      csvName: String,
      pdfName: String,
      data: IndexedSeq[Any],
      data2: IndexedSeq[Any]) = {
    
    // Create a directory to store results
    Files.createDirectories(resultsDir)
    
    // Write times to detection to CSV file for full data
    val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
    for (i <- 0 until data.length) {
      writer.write(s"${data(i).toString}, ${data2(i).toString}")
      writer.newLine
    }
    writer.close
    
    // Write R script which imports and plots data in a pdf
    val rScript = 
      s"""
        
      data = read.csv("$csvName")
      y1 = data[[1]]
      y2 = data[[2]]
      x = 1:length(y1)
            
      cmin = min(y1, y2)
      cmax = max(y1, y2)
      
      pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
      plot(x,y1,type="l",col="red")
  >   lines(x,y2,col="green")
      legend("topright", c["any alert", "consecutive alerts"])
      
      dev.off()
      """
      
    // Run the script in R and save the resulting PDF in the results directory
    ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
    
  }
      
  
  

}