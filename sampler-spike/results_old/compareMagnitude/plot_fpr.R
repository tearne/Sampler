data = read.csv("FPR.csv")
k = data[[1]]
fprAPHA = data[[2]]
fprAPHA_con = data[[3]]
k = c(0.2, k)
fprAPHA = c(0.01162, fprAPHA)
fprAPHA_con = c(2.7746E-4, fprAPHA_con)

data = read.csv("FPR.csv")
fprFarNew = data[[2]]
fprFarNew_con = data[[3]]
fprFarNew = c(0.09196, fprFarNew)
fprFarNew_con = c(0.01020, fprFarNew_con)

data = read.csv("FPR.csv")
fprStl = data[[2]]
fprStl_con = data[[3]]
fprStl = c(0.01470, fprStl)
fprStl_con = c(2.7888E-4, fprStl_con)

data = read.csv("FPR.csv")
fprAPHA_5 = data[[2]]
fprAPHA_5_con = data[[3]]
fprAPHA_5 = c(0.00585, fprAPHA_5)
fprAPHA_5_con = c(6.7469E-4, fprAPHA_5_con)

data = read.csv("FPR.csv")
fprFarNew_5 = data[[2]]
fprFarNew_5_con = data[[3]]
fprFarNew_5 = c(0.10138, fprFarNew_5)
fprFarNew_5_con = c(0.01646, fprFarNew_5_con)

data = read.csv("FPR.csv")
fprStl_5 = data[[2]]
fprStl_5_con = data[[3]]
fprStl_5 = c(0.01068, fprStl_5)
fprStl_5_con = c(3.20298E-4, fprStl_5_con)

cmin = min(fprAPHA,fprAPHA_5,fprFarNew,fprFarNew_5,fprStl,fprStl_5)
cmax = max(fprAPHA,fprAPHA_5,fprFarNew,fprFarNew_5,fprStl,fprStl_5)

plot(k,fprAPHA,type="l",lwd=6,lty=2,col=adjustcolor("green",0.8),ylim=c(cmin,cmax),
     xlab = "Magnitude of outbreak",
     ylab = "False positive rate")
lines(k,fprFarNew,lwd=6,lty=2,col=adjustcolor("cornflowerblue",0.8))
lines(k,fprStl,lwd=6,lty=2,col=adjustcolor("orange",0.8))

lines(k,fprAPHA_5,lwd=6,lty=3,col=adjustcolor("green",0.8))
lines(k,fprFarNew_5,lwd=6,lty=3,col=adjustcolor("cornflowerblue",0.8))
lines(k,fprStl_5,lwd=6,lty=3,col=adjustcolor("orange",0.8))

cmin_con = min(fprAPHA_con,fprAPHA_5_con,fprFarNew_con,fprFarNew_5_con,fprStl_con,fprStl_5_con)
cmax_con = max(fprAPHA_con,fprAPHA_5_con,fprFarNew_con,fprFarNew_5_con,fprStl_con,fprStl_5_con)

frame()
plot(k,fprAPHA_con,type="l",lwd=6,lty=2,col=adjustcolor("green",0.8),ylim=c(cmin_con,cmax_con),
     xlab = "Magnitude of outbreak",
     ylab = "False positive rate")
lines(k,fprFarNew_con,lwd=6,lty=2,col=adjustcolor("cornflowerblue",0.8))
lines(k,fprStl_con,lwd=6,lty=2,col=adjustcolor("orange",0.8))

lines(k,fprAPHA_5_con,lwd=6,lty=3,col=adjustcolor("green",0.8))
lines(k,fprFarNew_5_con,lwd=6,lty=3,col=adjustcolor("cornflowerblue",0.8))
lines(k,fprStl_5_con,lwd=6,lty=3,col=adjustcolor("orange",0.8))

# legend("topleft",
#        legend=c("APHA (12 years)", "Farrington (12 years)", "Modified APHA (12 years)", "APHA (5 years)", "Farrington (5 years)", "Modified APHA (5 years)"),
#        fill=c("green","cornflowerblue","orange","green","cornflowerblue","orange"),
#        lty=c(2,2,2,3,3,3))
