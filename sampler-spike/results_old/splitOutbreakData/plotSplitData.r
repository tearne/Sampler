setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/splitOutbreakData")

      
    data = read.csv("splitData.csv")
      
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
    
    pdf("splitOutbreakData.pdf", width=4.13, height=2.91) #A7 landscape paper
      
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
    
