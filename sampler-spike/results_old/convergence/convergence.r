setwd("/home/per024/ENVIRONMENT/workspaces/Sampler/sampler-spike/results/convergence")

      
    pdf("convergence.pdf", width=4.13, height=2.91) #A7 landscape paper

    data = read.csv("convergence.csv")
      
    pod = data[[1]]
      
    n = length(pod)
    
    plot(1:n, pod, type="l")
    
    dev.off()
    
