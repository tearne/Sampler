package farrington.study

import sampler.r.process.ScriptRunner
import java.nio.charset.Charset
import java.nio.file.Files
import farrington.core.algorithm.EDS
import sampler.r.rserve.RServeHelper
import farrington.core.simulate.SimulateOutbreakData
import farrington.core.simulate.BaselineData
import farrington.core.simulate.OutbreakData
import java.nio.file.Paths
import java.nio.file.Paths
import farrington.core.measures.Measures
import farrington.core.measures.AverageMeasures

object SplitOutbreak {
  
  //=======================
  // User-defined parameters
  
  // Number of months for which to simulate data:
  val nData = 462
  val endYear = 2014 
  
  // Choose "short" or "long" outbreaks
  //val outbreakLength = "short"
  val outbreakLength = "long"
  
  // Choose log-Normal or epidemic curve outbreak
  // val outbreakShape = "logNormal"
  val outbreakShape = "epidemicCurve"
  
  // Define end of each period
  //Baseline -> Pre-outbreak -> Outbreak -> Post-outbreak
  val endBaseline = 146
  val endPreOutbreak = 182
  val endOutbreak = 282
  
  val magnitude = 3
  
  // Identifiers for results files
  val csvName = "splitData.csv" // CSV file to store simulated data from Scala
  val scriptName = "plotSplitData.r" // R script to import the CSV and plot the data
  val pdfName = "splitOutbreakData.pdf" // PDF containing the plots
  
  // Choose directory to place resulting plot
  val resultsDir = Paths.get("results", "splitOutbreakData")
  
  //=======================
  // Simulate outbreak data
  
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
  val data = SimulateOutbreakData.addOutbreak(
    dataBaselineFull, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude
  )
  
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
  
  val measuresFull = Measures.allMeasures(dataFull, data.start, data.end)
  val measuresSplit1 = Measures.allMeasures(dataSplit1, data1.start, data1.end)
  val measuresSplit2 = Measures.allMeasures(dataSplit2, data2.start, data2.end)
  
  //=======================
  // Print relevant information to console:
  
  println("Total no. of months = " + nData)
  
  println("Baseline period starts at " + 1 + " = " + year(0) + "-" + month(0))
  println("Pre-outbreak period starts at " + endBaseline + " = " + year(endBaseline) + "-" + month(endBaseline))
  println("Outbreak period starts at " + endPreOutbreak + " = " + year(endPreOutbreak) + "-" + month(endPreOutbreak))
  println("Post-outbreak period starts at " + endOutbreak + " = " + year(endOutbreak) + "-" + month(endOutbreak))
  
  println("Outbreak begins at month " + data.start + " = " + year(data.start-1) + "-" + month(data.start-1))
  println("Outbreak occurs during months " + data.start + "-" + data.end)
  println("Outbreak counts (full) " + data.hist)
  println("Outbreak counts (split 1) " + data1.hist)
  println("Outbreak counts (split 2) " + data2.hist)
  
  println("Full data: ")
  Measures.print(measuresFull)
  
  println("Split 1 data: ")
  Measures.print(measuresSplit1)
  
  println("Split 2 data: ")
  Measures.print(measuresSplit2)
  
  //=======================
  // Visualisation
  
  // Create a directory to store results
  Files.createDirectories(resultsDir)
  
  // Write times to CSV file
  val writer = Files.newBufferedWriter(resultsDir.resolve(csvName), Charset.defaultCharset())
  writer.write("month, baseline, full, baseline1, split1, baseline2, split2, start, end")
  writer.newLine
  writer.write(s"${1.toString}, ${data.baseline(0).toString}, ${data.counts(0).toString}, ${data1.baseline(0).toString}, ${data1.counts(0).toString}, ${data2.baseline(0).toString}, ${data2.counts(0).toString}, ${data.start.toString}, ${data.end.toString}")
  writer.newLine
  for (i <- 1 until nData) {
    writer.write(s"${(i+1).toString}, ${data.baseline(i).toString}, ${data.counts(i).toString}, ${data1.baseline(i).toString}, ${data1.counts(i).toString}, ${data2.baseline(i).toString}, ${data2.counts(i).toString}")
    writer.newLine
  }
  writer.close
  
  // Write R script which imports and plots data in a pdf
  val rScript = 
    s"""
      
    data = read.csv("$csvName")
      
    month = data[["month"]]
    dataBaseline = data[["baseline"]]
    dataOutbreak = data[["full"]]
    dataBaseline1 = data[["baseline1"]]
    dataOutbreak1 = data[["split1"]]
    dataBaseline2 = data[["baseline2"]]
    dataOutbreak2 = data[["split2"]]
    start = data[["start"]][1]
    end = data[["end"]][1]
    
    counts = dataOutbreak - dataBaseline
    counts1 = dataOutbreak1 - dataBaseline1
    counts2 = dataOutbreak2 - dataBaseline2
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
    cmin = min(c(dataBaseline,dataOutbreak))
    cmax = max(c(dataBaseline,dataOutbreak))
    
    cmin2 = 0
    cmax2 = max(counts[c(start:end)])
    
    plot(month,dataBaseline,"l",
         ylim = c(cmin,cmax),
         main = "Simulated baseline data",
         xlab = "Months",
         ylab = "No. of cases")
    
    plot(month,dataOutbreak,"l",
         ylim = c(cmin,cmax),
         main = "Simulated outbreak data",
         xlab = "Months",
         ylab = "No. of cases")
    
    barplot(counts[c(start:end)],
          ylim = c(cmin2,cmax2),
          names.arg=as.character(month[c(start:end)]),
          main = "Outbreak data",
          xlab = "Months)",
          ylab = "No. of counts")
          
    barplot(counts1[c(start:end)],
          ylim = c(cmin2,cmax2),
          names.arg=as.character(month[c(start:end)]),
          main = "Outbreak data",
          xlab = "Months",
          ylab = "No. of counts")
          
    barplot(counts2[c(start:end)],
          ylim = c(cmin2,cmax2),
          names.arg=as.character(month[c(start:end)]),
          main = "Outbreak data",
          xlab = "Months",
          ylab = "No. of counts")
    
    dev.off()
    """
  
  // Run the script in R and save the resulting PDF in the results directory
  ScriptRunner.apply(rScript, resultsDir.resolve(scriptName))

}