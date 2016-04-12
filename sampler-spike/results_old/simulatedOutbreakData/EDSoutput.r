setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/simulatedOutbreakData")

      
    data = read.csv("EDSoutput.csv")

    month = data[["month"]]
    count = data[["count"]]
    thresh = data[["threshold"]]
    
    pdf("EDSoutput.pdf", width=8.27, height=5.83) #A5 landscape paper
    
    eds <- barplot(count,
          names.arg=as.character(month),
          main = "EDS results",
          xlab = "Time (months)",
          ylab = "No. of cases")
    lines(x = eds, y = thresh, type="l", col="red")
    
    dev.off()
    
