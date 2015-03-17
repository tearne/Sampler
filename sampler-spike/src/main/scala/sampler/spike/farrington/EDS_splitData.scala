package sampler.spike.farrington

import java.nio.file.Paths
import sampler.r.rserve.RServeHelper
import java.nio.file.Files
import java.nio.charset.Charset
import sampler.r.process.ScriptRunner

object EDS_splitData extends App{
  
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
  val dataBaseline1 = GenerateData.runBaseline(nData, endYear)
  val dataBaseline2 = GenerateData.runBaseline(nData, endYear)
  val year = dataBaseline1.year
  val month = dataBaseline1.month
  val mean1 = dataBaseline1.mean
  val mean2 = dataBaseline2.mean
  val baseline1 = dataBaseline1.baseline
  val baseline2 = dataBaseline2.baseline
  val baselineFull = baseline1.zip(baseline2).map(i => i._1 + i._2)
  
  val meanFull = mean1 + mean2
  val dataBaselineFull = BaselineResult(year, month, baselineFull, meanFull)
  
  // Simulate an outbreak using the full set of baseline data
  val data = GenerateData.addOutbreak(
    dataBaselineFull, outbreakShape, outbreakLength, endPreOutbreak, endOutbreak, magnitude
  )
  
  // Split the outbreak and add to the two smaller sets of baseline data:
  val (outbreak1, outbreak2) = GenerateData.splitOutbreak(data.hist, data.start)
  val data1 = GenerationResult(
      year,
      month,
      baseline1,
      GenerateData.addList(baseline1, outbreak1),
      outbreak1.map{ case (key, value) => (key - data.start + 1, value) },
      data.start,
      data.end)
  val data2 = GenerationResult(
      year,
      month,
      baseline2,
      GenerateData.addList(baseline2, outbreak2),
      outbreak2.map{ case (key, value) => (key - data.start + 1, value) },
      data.start,
      data.end)    
      
  // Run EDS for each data set
  RServeHelper.ensureRunning()
  val dataFull = EDS.run(data, endBaseline)    
  val dataSplit1 = EDS.run(data1, endBaseline)    
  val dataSplit2 = EDS.run(data2, endBaseline)
  RServeHelper.shutdown
  
  // Probability of detection
  val detectedFull = EDS.detected(dataFull, data.start, data.end)    
  val detectedSplit1 = EDS.detected(dataSplit1, data.start, data.end)    
  val detectedSplit2 = EDS.detected(dataSplit2, data.start, data.end)
  
  val POD = IndexedSeq(detectedFull, detectedSplit1, detectedSplit2)
  
  // Probability of consecutive detection
  val consecutiveFull = EDS.detectedConsecutive(dataFull, data.start, data.end)    
  val consecutiveSplit1 = EDS.detectedConsecutive(dataSplit1, data.start, data.end)    
  val consecutiveSplit2 = EDS.detectedConsecutive(dataSplit2, data.start, data.end)
  
  val POCD = IndexedSeq(consecutiveFull, consecutiveSplit1, consecutiveSplit2)
  
  // False Positive Rate
  val fprFull = EDS.falsePositiveRate(dataFull, data.start, data.end)
  val fprSplit1 = EDS.falsePositiveRate(dataSplit1, data.start, data.end)
  val fprSplit2 = EDS.falsePositiveRate(dataSplit2, data.start, data.end)
  
  val FPR = IndexedSeq(fprFull, fprSplit1, fprSplit2)
  
  // False Positive Rate (consecutive)
  val fprFullCon = EDS.fprConsecutive(dataFull, data.start, data.end)
  val fprSplit1Con = EDS.fprConsecutive(dataSplit1, data.start, data.end)
  val fprSplit2Con = EDS.fprConsecutive(dataSplit2, data.start, data.end)
  
  val FPRCon = IndexedSeq(fprFullCon, fprSplit1Con, fprSplit2Con)
  
  // Positive predictive value
  val ppvFull = EDS.positivePredictive(dataFull, data.start, data.end)
  val ppvSplit1 = EDS.positivePredictive(dataFull, data.start, data.end)
  val ppvSplit2 = EDS.positivePredictive(dataFull, data.start, data.end)
  
  val PPV = IndexedSeq(ppvFull, ppvSplit1, ppvSplit2)
  
  // Positive predictive value
  val ppvFullCon = EDS.ppvConsecutive(dataFull, data.start, data.end)
  val ppvSplit1Con = EDS.ppvConsecutive(dataFull, data.start, data.end)
  val ppvSplit2Con = EDS.ppvConsecutive(dataFull, data.start, data.end)
  
  val PPVCon = IndexedSeq(ppvFullCon, ppvSplit1Con, ppvSplit2Con)
  
  // Time To Detection
  val timesFull = EDS.timeToDetection(dataFull, data.start, data.end)
  val tFull = if (timesFull.length == 0) -1 else timesFull(0)
  
  val timesSplit1 = EDS.timeToDetection(dataSplit1, data.start, data.end)
  val tSplit1 = if (timesSplit1.length == 0) -1 else timesSplit1(0)
  
  val timesSplit2 = EDS.timeToDetection(dataSplit2, data.start, data.end)
  val tSplit2 = if (timesSplit2.length == 0) -1 else timesSplit2(0)
  
  val TTD = IndexedSeq(tFull, tSplit1, tSplit2)
  
  // Proportion of Outbreak Times Detected
  val potdFull = EDS.proportionDetected(dataFull, data.start, data.end)    
  val potdSplit1 = EDS.proportionDetected(dataSplit1, data.start, data.end)    
  val potdSplit2 = EDS.proportionDetected(dataSplit2, data.start, data.end)
  
  val POTD = IndexedSeq(potdFull, potdSplit1, potdSplit2)
  
  MeasureData(POD, POCD, FPR, FPRCon, PPV, PPVCon, TTD, POTD)
  
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
  
  println("Outbreak detected = " + POD)
  println("Outbreak detected consecutively = " + POCD)
  println("False positive rate = " + FPR)
  println("Time to detection = " + TTD)
  println("Proportion of outbreak times detected = " + POTD)
  
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