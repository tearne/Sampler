setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/compareFarrington")

    
    library(xtable)
    stats = read.csv("compareStats.csv")
    
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

    
