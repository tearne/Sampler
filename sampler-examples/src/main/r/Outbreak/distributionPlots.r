lapply(c("ggplot2", "reshape", "hexbin", "reshape2"), require, character.only=T)
            
setwd("~/Sampler/sampler-examples/results/Network/A1_2")
setwd("~/Sampler/sampler-examples/results/Network/A2_3")
setwd("~/Sampler/sampler-examples/results/Network/A3")
setwd("~/Sampler/sampler-examples/results/Network/B")
setwd("~/Sampler/sampler-examples/results/Network/B2")

posterior = cbind(
	read.csv("posterior.050.csv"),
	read.csv("posterior.049.csv"),
	read.csv("posterior.048.csv"),
	read.csv("posterior.047.csv"),
	read.csv("posterior.046.csv")
#	read.csv("posterior.045.csv"),
#	read.csv("posterior.044.csv"),
#	read.csv("posterior.043.csv"),
#	read.csv("posterior.042.csv"),
#	read.csv("posterior.041.csv")
}

# posterior = read.csv("posterior.050.csv")

truth = data.frame(value = c(0.6, 0.2), variable = c("Local", "Company"))

pdf("summary.pdf", width=8.26, height=2.91)
ggplot(melt(posterior), aes(x = value, linetype = variable)) + 
    geom_density(adjust = 2) +
    geom_vline(data = truth, aes(xintercept = value, linetype = variable), show_guide = T) +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), name = "Transmission Rate") +
    scale_linetype_discrete(name = "Spread Type")
    #ggtitle("Local Spread")
dev.off()
