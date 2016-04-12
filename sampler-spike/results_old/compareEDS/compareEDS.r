setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/compareEDS")

      
    data = read.csv("compareEDS.csv")

    month = data[["month"]]
    count = data[["count"]]
    APHA = data[["APHA"]]
    FarNew = data[["FarNew"]]
    Stl = data[["Stl"]]
    
    cmin = 0
    cmax = max(APHA, FarNew, Stl)
    
    pdf("compareEDS.pdf", width=8.27, height=5.83) #A5 landscape paper
    
    eds <- barplot(count,
          names.arg=as.character(month),
          main = "EDS results",
          xlab = "Time (months)",
          ylab = "No. of cases")
    lines(x = eds, y = APHA, type="l", col="red")
    lines(x = eds, y = FarNew, type="l", col="green")
    lines(x = eds, y = Stl, type="l", col="blue")
    
    dev.off()
    
