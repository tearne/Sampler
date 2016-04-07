eds = read.csv("EDSoutput.csv")

month = eds$month
count = eds$count
threshold = eds$threshold

cmin = min(count, threshold)
cmax = max(count, threshold)

start = 39
end = 139

bar <- barplot(count[start:end],
        names.arg = as.character(month[start:end]),
        col = rgb(0.3922,0.7,0.9294,alpha=0.7),
        ylim = c(cmin, cmax),
        main = "EDS",
        xlab = "Month",
        ylab = "Number of cases")

lines(x=bar, y=threshold[start:end], col=rgb(0.93,0.47,0.05,alpha=0.7), lwd=4)
legend(x=1, y=15, legend = "threshold", fill = rgb(0.93,0.47,0.05,alpha=0.7))