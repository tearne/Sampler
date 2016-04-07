data = read.csv("simData.csv")

month = data$month
baseline = data$baseline
count = data$outbreak
start = data$start
end = data$end

cmin = min(count)
cmax = max(count)

barplot(baseline[1:146],
   names.arg = as.character(month[1:146]),
   col = rgb(0.3922,0.7,0.9294,alpha=0.9),
   ylim = c(cmin, cmax),
   main = "Simulated data (baseline period)",
   xlab = "Month",
   ylab = "Number of cases")

barplot(baseline[147:182],
        names.arg = as.character(month[147:182]),
        col = rgb(0.3922,0.7,0.9294,alpha=0.9),
        ylim = c(cmin, cmax),
        main = "Simulated data (pre-outbreak period)",
        xlab = "Month",
        ylab = "Number of cases")

barplot(baseline[183:282],
        names.arg = as.character(month[183:282]),
        col = rgb(0.3922,0.7,0.9294,alpha=0.9),
        ylim = c(cmin, cmax),
        main = "Simulated data (outbreak period)",
        xlab = "Month",
        ylab = "Number of cases")

barplot(baseline[283:462],
        names.arg = as.character(month[283:462]),
        col = rgb(0.3922,0.7,0.9294,alpha=0.9),
        ylim = c(cmin, cmax),
        main = "Simulated data (post-outbreak period)",
        xlab = "Month",
        ylab = "Number of cases")

# plot(month, baseline, type="l",
#      col = rgb(0.098, 0.098, 0.4392, alpha=0.9),
#      main = "Simulated baseline data",
#      xlab = "Month",
#      ylab = "Number of cases")