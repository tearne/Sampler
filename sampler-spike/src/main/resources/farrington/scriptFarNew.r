#homeDir<-"//home/user/ENVIRONMENT/workspaces/workspace_scala/FarringtonTest/"
#fileDir<-"EDS"
#outDir<-paste(homeDir, "results", sep="")
#setwd(paste(homeDir,fileDir,sep=""))

 debug=F
 if(debug==T) {
     library(rjson)
     jsonIn = fromJSON(file="outR.json")
     nYearsBack = 5
     basedata=jsonIn$Baseline
     currentmth=jsonIn$Current$Month
     currentCount=jsonIn$Current$Incident
     startdte=jsonIn$StartDate
 }

require(rjson)

#allData = as.data.frame(fromJSON(readLines("tmp.json")))

########Model can not cope with expected[i] being too small and crashes (produces #NAN's).
########As it rounds it off and effectively tries to divide by zero
########Therefore I have set expected[i] to be an extremely small number in the #event that it is effectively zero
########This will not effect the validity of the results 

notZero=1*10^(-150)
resThresh=2.58      #put swithch in java to vary? New Farrington algorithm uses 2.58, old used 1
#######################################################
## DISPERSION 
## This function estimates the dispersion parameter using the formula described in Farrington #(1996)
#######################################################

disp<-function(i){
	calc<- w*( ( (basecont-expected)^2 )/expected)
	calc[expected<notZero]=notZero
	calc
}

###################################################
## DERIVE THRESHOLD VALUES	 
## This code is used to derive the threshold values using the formula described in Farrington #1996. 
## Two weights are derived -  "threshold" if the linear trend is included and "threshold2" if the
## the linear trend is not significant.
###################################################

# if model is to include linear trend then set 'trend0'=1, differnt calc of var
threshold <-function(z,trend0){
	ifelse(trend0==1, { 
				var=varalpha+(currentmth*currentmth)*varbeta+(2*currentmth*covariance)
			},{ var=varalpha })
	tao<-(dispersion*expectedc+var)/(expectedc^2)
	U<-expectedc*(1+(2/3)*z*(tao^0.5))^(3/2)
}

###########################################################################
###				MAIN MODEL				
###########################################################################

#allData = read.csv("/home/user/ENVIRONMENT/workspaces/workspace_scala/FarringtonTest/results/baselineData.txt")
#allData = as.data.frame(fromJSON(paste(readLines("r-in.json"), collapse="")))

baselineNum = length(basedata$basemth)
#baseData = allData[1:baselineNum,]
#currentCount = allData[nrow(allData),"Incidents"]
#currentmth = allData[nrow(allData), "MonthNum"]
basemth = basedata$basemth
basecont = basedata$basecont
tsSeasonal=0   #set to 0 unless use in stl
tsRandom=0     #set to 0 unless use in stl

#### Farrington New algorithm from R Surveillance library #-----------------------------------
library(sp)
library(xtable)
library(polyCub)
library(surveillance)
#TODO: start needs to be two numbers year, month.  Get this from basedata$basemth???

DataF=disProg2sts(create.disProg(1:baselineNum,basecont,state=basecont*0,start=c(startdte$year,startdte$month),freq=12)) #put in format necessar for farringtonFlexible algorithm

# define contol parameters: may want to make some of these switches in Scala code?
#b is number of years to use in calculations, w is number of months to use either side of current month
# weigthsThreshold=resThresh, i.e. threshold above which data are weighted 
#noPeriods is seasonality term to enable inclusion of more months

control1 <-  list(noPeriods=1,populationOffset=FALSE,
		fitFun="algo.farrington.fitGLM.flexible",
		b=nYearsBack,w=3,weightsThreshold=1,
		pastWeeksNotIncluded=3,
		pThresholdTrend=0.05,trend=TRUE,
		thresholdMethod="delta",alpha=0.1)
control2<-   list(noPeriods=10,populationOffset=FALSE,
		fitFun="algo.farrington.fitGLM.flexible",
		b=nYearsBack,w=7,weightsThreshold=2.58,
		pastWeeksNotIncluded=0,
		pThresholdTrend=1,trend=TRUE,
		thresholdMethod="delta",alpha=0.1)
if (modeFlag=="far") { 
	data1=farringtonFlexible(DataF,control=control1) 
} else  { 
	data1=farringtonFlexible(DataF,control=control2) 
} 

nrowF=nrow(data1)
#expectedF=rev(control(data1)$expected[nrowF:(nrowF-143)])
#expectedNewF=rev(control(data2)$expected[nrowF:(nrowF-143)])

#exceedF=rev(control(data1)$score[nrowF:(nrowF-143)])
#exceedNewF=rev(control(data2)$score[nrowF:(nrowF-143)])

#threshF=rev(upperbound(data1)[nrowF:(nrowF-143)])
#threshNewF=rev(upperbound(data2)[nrowF:(nrowF-143)])

#trendF=rev(control(data1)$trend[nrowF:(nrowF-143)])
#trendNewF=rev(control(data2)$trend[nrowF:(nrowF-143)])

#outputs
#expectedc1=control(data1)$expected
#thresh1=upperbound(data1)
#exceed=control(data1)$score
#trend=control(data1)$trend
#w=rep(0,times=length(basedata$basemth))

expectedc1=control(data1)$expected
thresh1=upperbound(data1)
exceed=control(data1)$score
trend=control(data1)$trend
w=rep(0,times=length(basedata$basemth))

	
#note if not taken out seasonality then nothing will change here.
#----output data --------------------------------------------------------------------
output = toJSON(list(
				"expected" = expectedc1,
				"threshold" = thresh1,
				"trend" = trend,
				"exceed" = exceed,
				"weights" = w
		))