package farrington.study

import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Paths
import scala.language.implicitConversions
import scala.language.postfixOps
import farrington.core.algorithm.EDS
import farrington.core.measures.AverageMeasures
import farrington.core.measures.Measures
import farrington.core.simulate.BaselineData
import farrington.core.simulate.OutbreakData
import farrington.core.simulate.SimulateOutbreakData
import sampler.r.process.ScriptRunner
import sampler.r.rserve.RServeHelper
import org.rosuda.REngine.Rserve.RConnection

/*
  =========
  NOTES:
  Script which simulates a number of simulated outbreak data sets
  and splits the outbreak for each set between two setse.
  The time to detect the outbreak is calculated for each simulation
  and the success rate for the full outbreak and each of the separated
  outbreaks is calculated.
     
  Uses default parameters to simulate baseline and outbreak data
  (Default is Scenario 14 in Noufaily et al., Statist. Med. 2013 (32) 1206-1222)
  
  =========
  AUTHOR:
  
  Author:    Teedah Saratoon
  Date:      04/03/2015
  Last edit: 04/04/2016
  
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
  
  
  */


object HiddenOutbreak extends App{
  
  //=======================
  // User-defined parameters
  
  // Number of sets of data to simulate
  val nSimulations = 250
  
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
  val magnitude = 0.6
      
  // Identifiers for results files
  val csv_full = "hiddenOutbreak_full.csv"     // CSV file to store simulated data from Scala
  val csv_split1 = "hiddenOutbreak_split1.csv"
  val csv_split2 = "hiddenOutbreak_split2.csv"
  val scriptName = "plotHiddenOutbreak.r"      // R script to import the CSV and plot the data
  val pdfName = "hiddenOutbreak.pdf"           // PDF containing the plots
  
  // Choose directory to place data and results
  val resultsDir = Paths.get("results", "hiddenOutbreak")
  
  //=======================
  // Simulate outbreak data and calculate measures

  RServeHelper.ensureRunning()
  val stats = (0 until nSimulations).par.map{i =>
  //val stats = (0 until nSimulations).map{i =>
    println(i)
    
    // Simulate two sets of baseline data and combine to create full set
    val dataBaseline1 = SimulateOutbreakData.runBaseline(nData, endYear)
    val dataBaseline2 = SimulateOutbreakData.runBaseline(nData, endYear)
    val year = dataBaseline1.year
    val month = dataBaseline1.month
    val mean1 = dataBaseline1.mean
    val mean2 = dataBaseline2.mean
    val baseline1 = dataBaseline1.baseline
    val baseline2 = dataBaseline2.baseline
    val baselineFull = baseline1.zip(baseline2).map(i => i._1 + i._2)
    
    val meanFull = (0 until mean1.length).map(i => mean1(i) + mean2(i))
    val dataBaselineFull = BaselineData(year, month, baselineFull, meanFull)
    
    // Simulate an outbreak using the full set of baseline data
    val data = SimulateOutbreakData.addOutbreak(dataBaselineFull, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude)
    
    // Split the outbreak and add to the two smaller sets of baseline data:
    val (outbreak1, outbreak2) = SimulateOutbreakData.splitOutbreak(data.hist, data.start)
    val data1 = OutbreakData(
        year,
        month,
        baseline1,
        SimulateOutbreakData.addList(baseline1, outbreak1),
        outbreak1.map{ case (key, value) => (key - data.start + 1, value) },
        data.start,
        data.end,
        data.min,
        data.max)
    val data2 = OutbreakData(
        year,
        month,
        baseline2,
        SimulateOutbreakData.addList(baseline2, outbreak2),
        outbreak2.map{ case (key, value) => (key - data.start + 1, value) },
        data.start,
        data.end,
        data.min,
        data.max)    
    
    // Run EDS for each data set
    RServeHelper.ensureRunning()
    val dataFull = EDS.run(data, endBaseline)
    val dataSplit1 = EDS.run(data1, endBaseline)    
    val dataSplit2 = EDS.run(data2, endBaseline)
    RServeHelper.shutdown
    
    // Calculate measures
    val measuresFull = Measures.allMeasures(dataFull, data.start, data.end)
    val measuresSplit1 = Measures.allMeasures(dataSplit1, data1.start, data1.end)
    val measuresSplit2 = Measures.allMeasures(dataSplit2, data2.start, data2.end)
    
    (measuresFull, measuresSplit1, measuresSplit2)
    
  }  
  RServeHelper.shutdown
  
  //=======================
  // Extract measures
  
  println("Magnitude of outbreak = " + magnitude)
  
  val stats_full = stats.map(_._1).toIndexedSeq
  val stats_split1 = stats.map(_._1).toIndexedSeq
  val stats_split2 = stats.map(_._1).toIndexedSeq
    
  val avgMeasuresFull = AverageMeasures.calculate(stats_full, nSimulations)
  val avgMeasuresSplit1 = AverageMeasures.calculate(stats_split1, nSimulations)
  val avgMeasuresSplit2 = AverageMeasures.calculate(stats_split2, nSimulations)  
  
  val POD_Split = 1 - ((1 - avgMeasuresSplit1.POD) * (1 - avgMeasuresSplit2.POD))   
  val POCD_Split = 1 - ((1 - avgMeasuresSplit1.POCD) * (1 - avgMeasuresSplit2.POCD))  
  
  //=======================
  // Print relevant information to console
      
  println("Full data: ")
  AverageMeasures.print(avgMeasuresFull)
  
  println("Split 1 data: ")
  AverageMeasures.print(avgMeasuresSplit1)
  
  println("Split 2 data: ")
  AverageMeasures.print(avgMeasuresSplit2)
  
  //=======================
  // Output and plot times to detection
  
  def ttdHist(stats: IndexedSeq[Measures]) = {
    stats.map(_.TTD).filter(i => i.length > 0).map(_.head).groupBy(w => w).mapValues(_.size).toList.sorted
  }
  
  // Time to detection
  val ttdHistFull = ttdHist(stats_full)
  val ttdHistSplit1 = ttdHist(stats_split1)
  val ttdHistSplit2 = ttdHist(stats_split2)
      
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to detection to CSV file for full data
  val writer = Files.newBufferedWriter(resultsDir.resolve(csv_full), Charset.defaultCharset())
  writer.write("time, count")
  writer.newLine
  for (i <- 0 until ttdHistFull.length) {
    writer.write(s"${ttdHistFull(i)._1.toString}, ${ttdHistFull(i)._2.toString}")
    writer.newLine
  }
  writer.close
  
  // Write times to detection to CSV file for split 1 data
  val writer2 = Files.newBufferedWriter(resultsDir.resolve(csv_split1), Charset.defaultCharset())
  writer2.write("time, count")
  writer2.newLine
  for (i <- 0 until ttdHistSplit1.length) {
    writer2.write(s"${ttdHistSplit1(i)._1.toString}, ${ttdHistSplit1(i)._2.toString}")
    writer2.newLine
  }
  writer2.close
  
  // Write times to detection to CSV file for split 2 data
  val writer3 = Files.newBufferedWriter(resultsDir.resolve(csv_split2), Charset.defaultCharset())
  writer3.write("time, count")
  writer3.newLine
  for (i <- 0 until ttdHistSplit2.length) {
    writer3.write(s"${ttdHistSplit2(i)._1.toString}, ${ttdHistSplit2(i)._2.toString}")
    writer3.newLine
  }
  writer3.close
  
  // Write R script which imports and plots data in a pdf
  val rScript = 
    s"""
      
    full = read.csv("$csv_full")
    split1 = read.csv("$csv_split1")
    split2 = read.csv("$csv_split2")
          
    cmin = 0
    cmax = max(full[["count"]], split1[["count"]], split2[["count"]])
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
    
    barplot(full[["count"]],
          names.arg = as.character(full[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (full set)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(split1[["count"]],
          names.arg = as.character(split1[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (split 1)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(split2[["count"]],
          names.arg = as.character(split2[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (split 2)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))
  
  println("Time to detection saved to " + pdfName)

  //=======================
  // Function definitions
  
  def successRate(nTotal: Int, hist: List[(Int, Int)]) = {
    val nMisses =
      if (hist.count(i => i._1 == -1) == 0) 100
      else hist(0)._2
    ((nTotal - nMisses).toDouble / nTotal) * 100
  }

}