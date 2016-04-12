package farrington.core.script

object CreateRScript {
  
  def plotSimulatedData(csvName: String, pdfName: String) = {
    s"""
      
    data = read.csv("$csvName")
      
    month = data[["month"]]
    dataBaseline = data[["baseline"]]
    dataOutbreak = data[["outbreak"]]
    start = data[["start"]][1]
    end = data[["end"]][1]
    
    counts = dataOutbreak - dataBaseline
    
    pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
    cmin = min(c(dataBaseline,dataOutbreak))
    cmax = max(c(dataBaseline,dataOutbreak))
    
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
          names.arg=as.character(month[c(start:end)]),
          main = "Outbreak",
          xlab = "Month",
          ylab = "No. of outbreak cases")
    
    dev.off()
    """
  }
  
  def plotComparison(csvName: String, pdfName: String) = {
    s"""
      
    data = read.csv("$csvName")

    month = data[["month"]]
    count = data[["count"]]
    APHA = data[["APHA"]]
    FarNew = data[["FarNew"]]
    Stl = data[["Stl"]]
    
    cmin = 0
    cmax = max(APHA, FarNew, Stl)
    
    pdf("$pdfName", width=8.27, height=5.83) #A5 landscape paper
    
    eds <- barplot(count,
          names.arg=as.character(month),
          main = "EDS results",
          xlab = "Time (months)",
          ylab = "No. of cases")
    lines(x = eds, y = APHA, type="l", col="red")
    lines(x = eds, y = FarNew, type="l", col="green")
    lines(x = eds, y = Stl, type="l", col="blue")
    
    dev.off()
    """

  }
  
  def plotTTD(csvName: String, pdfName: String) = {
    s"""        
      data = read.csv("$csvName")            
      cmin = 0
      cmax = max(APHA[["count"]], FarNew[["count"]], Stl[["count"]])      
      pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
      barplot(data[["count"]],
            names.arg = as.character(data[["time"]]),
            ylim = c(cmin, cmax),
            main = "Time to detection",
            xlab = "Time to detect outbreak (months)",
            ylab = "No. of counts")
      
      dev.off()
      """
  }
  
  def plotTwo(yLabel: String, csvName: String, pdfName: String) = {
    s"""
        
      data = read.csv("$csvName")
      x = data[[1]]
      y1 = data[[2]]
      y2 = data[[3]]
            
      cmin = min(y1, y2)
      cmax = max(y1, y2)
      
      pdf("$pdfName", width=4.13, height=2.91) #A7 landscape paper
      
      plot(x, y1, type="l", col=2, lwd=5, ylim = c(cmin, cmax), xlab="Magnitude of outbreak", ylab="$yLabel", )
      lines(x, y2, col=3, lwd=5)
      legend("topright", xpd=TRUE, inset=c(0.2,-0.65), legend=c("any alert", "consecutive alerts"), fill=2:3)
      
      dev.off()
      """
  }
  
  def statsToTable(csvStats: String) = {
    s"""
    
    library(xtable)
    stats = read.csv("$csvStats")
    
    mode = stats[["mode"]]
    pod = stats[["pod"]]
    pocd = stats[["pocd"]]
    fpr = stats[["fpr"]]
    fprc = stats[["fprc"]]
    ppv = stats[["ppv"]]
    ppvc = stats[["ppvc"]]
    ttd = stats[["ttd"]]
    ttcd = stats[["ttcd"]]
    potd = stats[["potd"]]
    
    dAll = data.frame(POD = pod, FPR = fpr, PPV = ppv, TTD = ttd, POTD = potd, row.names = mode)
    print(xtable(dAll), type="html", file="compareStats_All.html")
    
    dCon = data.frame(POD = pocd, FPR = fprc, PPV = ppvc, TTD = ttcd,  row.names = mode)
    print(xtable(dCon), type="html", file="compareStats_Consecutive.html")

    """
  }
  
  
}