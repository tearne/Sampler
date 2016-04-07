library(ggplot2)

full = read.csv("hiddenOutbreak_full.csv")
split1 = read.csv("hiddenOutbreak_split1.csv")
split2 = read.csv("hiddenOutbreak_split2.csv")

tFull = full$time[-1]
tSplit1 = split1$time[-1]
tSplit2 = split2$time[-1]

cFull = full$count[-1]
cSplit1 = split1$count[-1]
cSplit2 = split2$count[-1]

cmin = 0
cmax = max(cFull, cSplit1, cSplit2)

tmin = min(tFull, tSplit1, tSplit2)
tmax = max(tFull, tSplit1, tSplit2)

dat <- data.frame(
  group = factor(c("Full","Full","Full","Full","County 1","County 1","County 1","County 1","County 1","County 2","County 2","County 2","County 2"), levels=c("Full","County 1","County 2")),
  time = factor(c(tFull,tSplit1,tSplit2), levels=c(0,1,2,3,5)),
  counts = c(cFull,cSplit1,cSplit2)
)

pdf("ttd.pdf", width=4.13, height=2.91) #A7 landscape paper

ggplot(data=dat, aes(x=time, y=counts, fill = group)) +
  geom_bar(stat="identity",position=position_dodge())

# barplot(cFull,
#         names.arg = as.character(cFull),
#         ylim = c(cmin, cmax),
#         main = "Time to detection (full set)",
#         xlab = "Time to detect outbreak (months)",
#         ylab = "No. of counts")
# 
# barplot(cSplit1,
#         names.arg = as.character(tSplit1),
#         ylim = c(cmin, cmax),
#         main = "Time to detection (split 1)",
#         xlab = "Time to detect outbreak (months)",
#         ylab = "No. of counts")
# 
# barplot(cSplit2,
#         names.arg = as.character(cSplit2),
#         ylim = c(cmin, cmax),
#         main = "Time to detection (split 2)",
#         xlab = "Time to detect outbreak (months)",
#         ylab = "No. of counts")

dev.off()