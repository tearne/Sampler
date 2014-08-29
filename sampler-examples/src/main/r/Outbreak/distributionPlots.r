lapply(c("ggplot2", "reshape", "hexbin", "reshape2"), require, character.only=T)
            
wd = "~/workspace/RD0067/results/Network/0.4-0.1/PartialSeq"
fileIds = 80:99
load = function(fileIndex){
	fileName = paste("posterior",fileIndex, "csv", sep=".")
	read.csv(paste(wd, fileName, sep="/"))
}

sapply(fileIds, load)


stats = function(values, name){
    table = c(
        quantile(values, c(0.05, 0.5, 0.95)),
        mean = mean(values)
    )
    df = data.frame(variable = names(table), param = name, value = as.vector(table))
    df
}
   
   
data = rbind(
    stats(posteriorObs$Local, "Local-NoSeq"),
    stats(posteriorSeq$Local, "Local-Seq"),
    stats(posteriorObs$Company, "Company-NoSeq"),
    stats(posteriorSeq$Company, "Company-Seq")
)
print(dcast(data, param ~ variable))
   
localSpread = data.frame(WithNGS = posteriorSeq$Local, NoNGS = posteriorObs$Local)    
companySpread = data.frame(WithNGS = posteriorSeq$Company, NoNGS = posteriorObs$Company)      

pdf("results/Network/plots.pdf", width=4.13, height=2.91) #A7 landscape paper
ggplot(melt(localSpread), aes(x = value, linetype = variable)) + 
    geom_density() +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), name = "Transmission Rate") +
    ggtitle("Local Spread")

ggplot(melt(companySpread), aes(x = value, linetype = variable)) + 
    geom_density() +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1), name = "Transmission Rate") +
    ggtitle("Company Spread")
dev.off()