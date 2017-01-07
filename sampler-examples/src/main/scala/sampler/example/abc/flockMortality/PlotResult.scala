package flockMortalityExample

import sampler.r.script.RScript
import java.nio.file.Paths

object PlotResult extends App {
  
  //=======================
  // PLOT POSTERIOR
  
  val outDir = Paths.get("dataOut")  
    
  val rScript = 
"""
lapply(c("ggplot2", "scales", "reshape2", "jsonlite"), require, character.only=T)

sheds = fromJSON("result.json")$observed$id

population = as.data.frame(fromJSON("population.json")$'particle-summary')

sampledPosterior = population[sample(nrow(population), replace = T, 1000, prob = population$weight),]

pdf("density.pdf", width=4.13, height=2.91) #A7 landscape paper

ggplot(melt(subset(sampledPosterior, select = c(beta, eta, gamma, delta, sigma, sigma2))), aes(x=value, colour=variable)) +
  geom_density()

if (length(sheds) == 1) {
  
  json = paste("Shed", sheds, "_fitted.json", sep="")
  
  properties = as.data.frame(fromJSON(json)$properties)
  
  fitted = as.data.frame(fromJSON(json)$fitted)
  observed = as.data.frame(fromJSON(json)$observed)
  
  day = properties$days
  
  fitEggs = fitted$eggs
  obsEggs = observed$eggs
  fitDead = fitted$dead
  obsDead = observed$dead
  
  fitCumDead = cumsum(fitDead)
  obsCumDead = cumsum(obsDead)
  
  offset = "offset"
  
  filename = paste("Shed", sheds, "_fit.pdf", sep="")
  
  print(ggplot(melt(sampledPosterior[,offset]), aes(x=factor(value))) + 
      geom_bar() +
      scale_x_discrete("Offset (days)"))
  
  print(ggplot(fitted, aes(x=day)) + 
      geom_line(aes(y=fitEggs)) +
      geom_point(aes(y=obsEggs)) +
      scale_y_continuous("Egg Production"))
  
  print(ggplot(fitted, aes(x=day)) +
      geom_line(aes(y=fitDead)) +
      geom_point(aes(y=obsDead)) +
      scale_y_continuous("Daily mortality"))
  
  print(ggplot(fitted, aes(x=day)) +
      geom_line(aes(y=fitCumDead)) +
      geom_point(aes(y=obsCumDead)) +
      scale_y_continuous("Cumulative mortality"))
  
} else {
  
  for (shed in sheds) {
  
    json = paste("Shed", shed, "_fitted.json", sep="")
    
    properties = as.data.frame(fromJSON(json)$properties)
    
    fitted = as.data.frame(fromJSON(json)$fitted)
    observed = as.data.frame(fromJSON(json)$observed)
    
    day = properties$days
    
    fitEggs = fitted$eggs
    obsEggs = observed$eggs
    fitDead = fitted$dead
    obsDead = observed$dead
    
    fitCumDead = cumsum(fitDead)
    obsCumDead = cumsum(obsDead)
    
    offset = paste("offset.", shed, sep="")
    
    filename = paste("Shed", shed, "_fit.pdf", sep="")
    
    print(ggplot(melt(sampledPosterior[,offset]), aes(x=factor(value))) + 
            geom_bar() +
            scale_x_discrete("Offset (days)"))
    
    print(ggplot(fitted, aes(x=day)) + 
            geom_line(aes(y=fitEggs)) +
            geom_point(aes(y=obsEggs)) +
            scale_y_continuous("Egg Production"))
    
    print(ggplot(fitted, aes(x=day)) +
            geom_line(aes(y=fitDead)) +
            geom_point(aes(y=obsDead)) +
            scale_y_continuous("Daily mortality"))
    
    print(ggplot(fitted, aes(x=day)) +
            geom_line(aes(y=fitCumDead)) +
            geom_point(aes(y=obsCumDead)) +
            scale_y_continuous("Cumulative mortality"))
    
  }
}

dev.off()
"""

  RScript(rScript, outDir.resolve("script.r"))

  val multiplot = this.getClass.getClassLoader.getResource("multiplot.R").getPath

  val rScript2 =
"""
lapply(c("ggplot2", "scales", "reshape2", "jsonlite"), require, character.only=T)
source("multiplot")

# IMPORT DATA -------------------------------------------------------------

# Read in truth and posterior results
results = fromJSON("result.json")
posterior = results$population$'particle-summary'
#truth = fromJSON("truth.json")

# Separate offset data from posterior
offset = melt(t(as.data.frame(posterior$offset)))
offset[[1]] <- NULL
colnames(offset) = c("shed", "value")
offset$shed = factor(offset$shed)
posterior$offset <- NULL
posterior$weight <- NULL
posterior = as.data.frame(posterior)

######
# Kernel density plots

pdf("posterior.pdf", width=8.26, height=5.82) #A5 landscape paper

p0 <- ggplot(melt(posterior$beta), aes(x=value)) +
  geom_density(size = 0.5, fill = 5, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1)) +
  ggtitle("Transmission rate")
 
p1 <- ggplot(melt(1/posterior$eta), aes(x=value)) +
  geom_density(size = 0.5, fill = 7, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10)) +
  ggtitle("Latent period (days)") 

p2 <- ggplot(melt(1/posterior$gamma), aes(x=value)) +
  geom_density(size = 0.5, fill = 2, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10)) +
  ggtitle("Infectious period (days)")

p3 <- ggplot(melt(posterior$delta), aes(x=value)) +
  geom_density(size = 0.5, fill = 4, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1)) +
  ggtitle("Mortality rate")

p4 <- ggplot(melt(posterior$sigma), aes(x=value)) +
  geom_density(size = 0.5, fill = 3, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1)) +
  ggtitle("Egg production (infected)")

p5 <- ggplot(melt(posterior$sigma2), aes(x=value)) +
  geom_density(size = 0.5, fill = 6, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 1)) +
  ggtitle("Egg production (recovered)")

#p6 <- ggplot(offset, aes(x=factor(value), fill = shed)) +
#  geom_bar() +
#  ggtitle("Start of infection")

#multiplot(p0, p2, p3, p4, p5, p6, cols=3)
#multiplot(p0, p2, p3, p4, p5, cols=3)
multiplot(p0, p1, p2, p3, p4, p5, cols=3)

dev.off()
""".replaceFirst("multiplot", multiplot)
  RScript(rScript2, outDir.resolve("posterior.r"))

}