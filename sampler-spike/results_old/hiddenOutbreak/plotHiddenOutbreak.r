setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/hiddenOutbreak")

      
    full = read.csv("hiddenOutbreak_full.csv")
    split1 = read.csv("hiddenOutbreak_split1.csv")
    split2 = read.csv("hiddenOutbreak_split2.csv")
          
    cmin = 0
    cmax = max(full[["count"]], split1[["count"]], split2[["count"]])
    
    pdf("hiddenOutbreak.pdf", width=4.13, height=2.91) #A7 landscape paper
    
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
    
