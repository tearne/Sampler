require(ggplot2)
require(plyr)
require(reshape)

getwd()
setwd("/Users/o/_DUK/CODE/workspace/Sampler")


#
# The model we are going to use for fitting
# Note how the noise is neither normal nor heteroscedastic
#
noise = function(x){
	normal = rnorm(1,0, x)
	normal^2
}

m=30
v=2

model = function(x){
	m*x + sqrt(x)*noise(v)
}

#
# What does the noise look like?
#
noiseObs = sapply(1:1000, function(x){noise(v)})
ggplot(as.data.frame(noiseObs), aes(x=noiseObs)) + geom_density()

#
# What does the model look like?
#
xObs = sample(1:100, 1000, replace=T)
yObs = sapply(xObs, function(x){model(x)})
data = data.frame(xObs, yObs)
ggplot(data, aes(x=xObs, y=yObs)) + geom_point(size = 0.5)

#
# Generate few observations to try fitting using Sampler 
# against the known model
#
xObs = sample(1:100, 3, replace=T)
yObs = sapply(xObs, function(x){model(x)})

particles = read.csv("example/simpleABC/data/30.517578125.csv")
ggplot(melt(particles), aes(x=value, colour=variable)) + geom_density()

particles = read.csv("example/simplePrevABC/data/30.517578125.csv")
ggplot(particles, aes(x=m, y=v)) + geom_hex()
