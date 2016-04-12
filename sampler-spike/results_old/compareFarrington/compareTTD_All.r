setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/compareFarrington")

      
    APHA = read.csv("compareTTD_All_APHA.csv")
    FarNew = read.csv("compareTTD_All_FarNew.csv")
    Stl = read.csv("compareTTD_All_Stl.csv")
          
    cmin = 0
    cmax = max(APHA[["count"]], FarNew[["count"]], Stl[["count"]])
    
    pdf("compareTTD_All.pdf", width=4.13, height=2.91) #A7 landscape paper
    
    barplot(APHA[["count"]],
          names.arg = as.character(APHA[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (APHA)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(FarNew[["count"]],
          names.arg = as.character(FarNew[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (FarNew)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
          
    barplot(Stl[["count"]],
          names.arg = as.character(Stl[["time"]]),
          ylim = c(cmin, cmax),
          main = "Time to detection (Stl)",
          xlab = "Time to detect outbreak (months)",
          ylab = "No. of counts")
    
    dev.off()
    
