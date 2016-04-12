data = read.csv("POD.csv")
k = data[[1]]
podAPHA = data[[2]]
podAPHA_con = data[[3]]
k = c(0.2, k)
podAPHA = c(0.2333333333, podAPHA)
podAPHA_con = c(0.02666666, podAPHA_con)

data = read.csv("POD.csv")
podFarNew = data[[2]]
podFarNew_con = data[[3]]
podFarNew = c(0.666666666, podFarNew)
podFarNew_con = c(0.20666666666, podFarNew_con)

data = read.csv("POD.csv")
podStl = data[[2]]
podStl_con = data[[3]]
podStl = c(0.25333333333, podStl)
podStl_con = c(0.02, podStl_con)

data = read.csv("POD.csv")
podAPHA_5 = data[[2]]
podAPHA_5_con = data[[3]]
podAPHA_5 = c(0.1133333333, podAPHA_5)
podAPHA_5_con = c(0.0133, podAPHA_5_con)

data = read.csv("POD.csv")
podFarNew_5 = data[[2]]
podFarNew_5_con = data[[3]]
podFarNew_5 = c(0.74, podFarNew_5)
podFarNew_5_con = c(0.20666666666, podFarNew_5_con)

data = read.csv("POD.csv")
podStl_5 = data[[2]]
podStl_5_con = data[[3]]
podStl_5 = c(0.14666666666, podStl_5)
podStl_5_con = c(0.01333333333, podStl_5_con)

plot(k,podAPHA,type="l",lwd=6,lty=2,col=adjustcolor("green",0.8),ylim=c(0,1),
     xlab = "Magnitude of outbreak",
     ylab = "Probability of detection")
lines(k,podFarNew,lwd=6,lty=2,col=adjustcolor("cornflowerblue",0.8))
lines(k,podStl,lwd=6,lty=2,col=adjustcolor("orange",0.8))

lines(k,podAPHA_5,lwd=6,lty=3,col=adjustcolor("green",0.8))
lines(k,podFarNew_5,lwd=6,lty=3,col=adjustcolor("cornflowerblue",0.8))
lines(k,podStl_5,lwd=6,lty=3,col=adjustcolor("orange",0.8))

legend("bottomright",
       legend=c("APHA (12 years)", "Farrington (12 years)", "Modified APHA (12 years)", "APHA (5 years)", "Farrington (5 years)", "Modified APHA (5 years)"),
       fill=c("green","cornflowerblue","orange","green","cornflowerblue","orange"),
       lty=c(2,2,2,3,3,3))

plot(k,podAPHA_con,type="l",lwd=6,lty=2,col=adjustcolor("green",0.8),ylim=c(0,1),
     xlab = "Magnitude of outbreak",
     ylab = "Probability of detection")
lines(k,podFarNew_con,lwd=6,lty=2,col=adjustcolor("cornflowerblue",0.8))
lines(k,podStl_con,lwd=6,lty=2,col=adjustcolor("orange",0.8))

lines(k,podAPHA_5_con,lwd=6,lty=3,col=adjustcolor("green",0.8))
lines(k,podFarNew_5_con,lwd=6,lty=3,col=adjustcolor("cornflowerblue",0.8))
lines(k,podStl_5_con,lwd=6,lty=3,col=adjustcolor("orange",0.8))
