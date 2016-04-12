data = read.csv("PPV.csv")
k = data[[1]]
ppvAPHA = data[[2]]
ppvAPHA_con = data[[3]]
k = c(0.2, k)
ppvAPHA = c(0.08175, ppvAPHA)
ppvAPHA_con = c(0.91333, ppvAPHA_con)

data = read.csv("PPV.csv")
ppvFarNew = data[[2]]
ppvFarNew_con = data[[3]]
ppvFarNew = c(0.04091, ppvFarNew)
ppvFarNew_con = c(0.10779, ppvFarNew_con)

data = read.csv("PPV.csv")
ppvStl = data[[2]]
ppvStl_con = data[[3]]
ppvStl = c(0.05950, ppvStl)
ppvStl_con = c(0.92, ppvStl_con)

data = read.csv("PPV.csv")
ppvAPHA_5 = data[[2]]
ppvAPHA_5_con = data[[3]]
ppvAPHA_5 = c(0.10433, ppvAPHA_5)
ppvAPHA_5_con = c(0.97333, ppvAPHA_5_con)

data = read.csv("PPV.csv")
ppvFarNew_5 = data[[2]]
ppvFarNew_5_con = data[[3]]
ppvFarNew_5 = c(0.03522, ppvFarNew_5)
ppvFarNew_5_con = c(0.05983, ppvFarNew_5_con)

data = read.csv("PPV.csv")
ppvStl_5 = data[[2]]
ppvStl_5_con = data[[3]]
ppvStl_5 = c(0.04888, ppvStl_5)
ppvStl_5_con = c(0.88, ppvStl_5_con)

cmin = min(ppvAPHA,ppvAPHA_5,ppvFarNew,ppvFarNew_5,ppvStl,ppvStl_5)
cmax = max(ppvAPHA,ppvAPHA_5,ppvFarNew,ppvFarNew_5,ppvStl,ppvStl_5)

plot(k,ppvAPHA,type="l",lwd=6,lty=2,col=adjustcolor("green",0.8),ylim=c(cmin, cmax),
     xlab = "Magnitude of outbreak",
     ylab = "Positive predictive value")
lines(k,ppvFarNew,lwd=6,lty=2,col=adjustcolor("cornflowerblue",0.8))
lines(k,ppvStl,lwd=6,lty=2,col=adjustcolor("orange",0.8))

lines(k,ppvAPHA_5,lwd=6,lty=3,col=adjustcolor("green",0.8))
lines(k,ppvFarNew_5,lwd=6,lty=3,col=adjustcolor("cornflowerblue",0.8))
lines(k,ppvStl_5,lwd=6,lty=3,col=adjustcolor("orange",0.8))

legend("topleft",
       legend=c("APHA (12 years)", "Farrington (12 years)", "Modified APHA (12 years)", "APHA (5 years)", "Farrington (5 years)", "Modified APHA (5 years)"),
       fill=c("green","cornflowerblue","orange","green","cornflowerblue","orange"),
       lty=c(2,2,2,3,3,3))

cmin_con = min(ppvAPHA_con,ppvAPHA_5_con,ppvFarNew_con,ppvFarNew_5_con,ppvStl_con,ppvStl_5_con)
cmax_con = max(ppvAPHA_con,ppvAPHA_5_con,ppvFarNew_con,ppvFarNew_5_con,ppvStl_con,ppvStl_5_con)

plot(k,ppvAPHA_con,type="l",lwd=6,lty=2,col=adjustcolor("green",0.8),ylim=c(cmin_con, cmax_con),
     xlab = "Magnitude of outbreak",
     ylab = "Positive predictive value")
lines(k,ppvFarNew_con,lwd=6,lty=2,col=adjustcolor("cornflowerblue",0.8))
lines(k,ppvStl_con,lwd=6,lty=2,col=adjustcolor("orange",0.8))

lines(k,ppvAPHA_5_con,lwd=6,lty=3,col=adjustcolor("green",0.8))
lines(k,ppvFarNew_5_con,lwd=6,lty=3,col=adjustcolor("cornflowerblue",0.8))
lines(k,ppvStl_5_con,lwd=6,lty=3,col=adjustcolor("orange",0.8))