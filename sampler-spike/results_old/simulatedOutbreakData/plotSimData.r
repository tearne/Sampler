setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/simulatedOutbreakData")

      
    data = read.csv("simData.csv")
      
    month = data[["month"]]
    dataBaseline = data[["baseline"]]
    dataOutbreak = data[["outbreak"]]
    start = data[["start"]][1]
    end = data[["end"]][1]
    
    counts = dataOutbreak - dataBaseline
    
    pdf("simulatedOutbreakData.pdf", width=4.13, height=2.91) #A7 landscape paper
      
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
    
