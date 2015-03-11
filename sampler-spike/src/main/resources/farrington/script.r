#homeDir<-"//home/user/ENVIRONMENT/workspaces/workspace_scala/FarringtonTest/"
#fileDir<-"EDS"
#outDir<-paste(homeDir, "results", sep="")
#setwd(paste(homeDir,fileDir,sep=""))

## debug=F
## if(debug==T) {
##     library(rjson)
##     jsonIn = fromJSON(file="outR.json")
##     modeFlag = "stl"
##     basedata=jsonIn$Baseline
##     currentmth=jsonIn$Current$Month
##     currentCount=jsonIn$Current$Incident
##     startdte=jsonIn$StartDate
## }


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
if  ( modeFlag %in% c("far", "farNew") )  {
	
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
			b=5,w=3,weightsThreshold=1,
			pastWeeksNotIncluded=3,
			pThresholdTrend=0.05,trend=TRUE,
			thresholdMethod="delta",alpha=0.1)
	control2<-   list(noPeriods=10,populationOffset=FALSE,
			fitFun="algo.farrington.fitGLM.flexible",
			b=5,w=7,weightsThreshold=2.58,
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
		expectedc1=control(data1)$expected[nrowF]
		thresh1=upperbound(data1)[nrowF]
		exceed=control(data1)$score[nrowF]
		trend=control(data1)$trend[nrowF]
		w=rep(0,times=length(basedata$basemth))

	#plot(data1) 
	#TODO: quit code here
} else {
#----- decomposition for full dataset use ------------------------------------------------
#if ( modeFlag %in% c("stl", "apha") )  {
    if (modeFlag =="stl") {    
		tsData=ts(basecont,frequency=12,start=c(startdte$year,startdte$month)) 	#turn data into time series object
		tsSplit=stl(tsData,s.window=12)                  #splits data into 'seasonal,'trend' and 'random' components
		basecont=as.vector(tsSplit$time.series[,2]) +as.vector(tsSplit$time.series[,3]) #redefine basecont as just the trend and random component
		tsSeasonal=as.vector(tsSplit$time.series[,1])
		tsRandom=0#as.vector(tsSplit$time.series[,3]) #including in basecont so set =0 here
	}
	basecont[basecont<=0]=0 #cant have -ve values for glm fit - shouldnt be -ve anyway
	
	#--------------------------------------------------------------------------------------------------
	#basedata<-data.frame(basecont, basemth)
	# currentCount<-Data2[currentmth, 2+2]  	#LEAVE AS [,2+2] so only look at INCIDENTS. If wish to look at isolations change to [,1+1]. 
	n=length(basedata$basemth)
	basemth<-c(1:n)
	w<-rep(1,times=n)					#Set weights = 1 
	#----fit model with no linear trend----------------------------------------------------------------------------------------
	model0<-glm(formula=basecont~1, family=quasipoisson(link=log), weights=w, data=basedata)  #model with no linear trend
		param0<-coef(model0)
		coeff0<-summary(model0)$coeff
	#--------fit full model-------------------------------------------------------------------------------------------------
	modelF<-glm(formula=basecont~basemth, family=quasipoisson(link=log), weights=w, data=basedata)	#Fit model
		paramF<-coef(modelF)
		hatF<-lm.influence(modelF)$hat
	#---calculate weights----------------     
	expected<-exp(paramF[1]+(basemth*paramF[2]))  	#Estimate expected values
	p<-2						#Number of esitmated parameters
	calc<-sum(disp())					#First calculation of dispersion parameter
	calc2<-1/(n-p)
	dispersion<-if((calc2*calc)>1) calc2*calc else 1		#Calculate dispersion parameter final     
	residuals1= (3/(2*(dispersion)^(1/2)))*(((basecont)^(2/3))-((expected)^(2/3))) / (((expected)^(1/6))*((1-hatF)^(1/2)))         
		residuals1[expected<notZero]=notZero 
	m<-length(residuals1[residuals1>resThresh])
	k=rep(0,times=n)
		k[residuals1>resThresh]=residuals1[residuals1>resThresh]^-2
	y<-n/(sum(k)+(n-m))				#Calculate the weights
		w=k*y
		w[w==0]=y     
	#----fit model with weights--------------------
	modelW<-glm(formula=basecont~basemth, family=quasipoisson(link=log), weights=w, data=basedata)   
		paramW<-coef(modelW)
		if( any(is.na(paramW)) ) {modelW=modelF ;  paramW=coef(modelW) }
		coeffW<-summary(modelW)$coeff
		covaW<-vcov(modelW) 
	#---------------------------------------------------
	expected<-exp(paramW[1]+(basemth*paramW[2]))  	#Estimate expected values again
	expectedc<-exp(paramW[1]+(currentmth*paramW[2]))	#Estimate expected value for current month again	   
	varalpha<-coeffW[1,2]*coeffW[1,2]
	varbeta<-coeffW[2,2]*coeffW[2,2]
	covariance<-covaW[1,2]
	
	calc<-sum(disp())					#Calculate the first part of dispersion parameter again
	calc2<-1/(n-p)
	dispersion<-if((calc2*calc)>1) calc2*calc else 1		#Recalculate the dispersion parameter
	tvalue<-coeffW[2]/coeffW[2,2]				#T-value for beta
	df<-n-p-1					#Degrees freedom
	ttest<-qt(0.95,df)					#T-test for the significance of linear trend
	z<-1.96  
	U<-threshold(z,1)		#Calculate the threshold 
	trend=1          #denotes trend is included
	if (!( (tvalue>ttest | tvalue < -ttest) & expectedc<=max(basecont)))  { 
	    expected<-expected*0+exp(param0)
		expectedc<-exp(param0)
		trend=0
		p<-1
		calc<-sum(disp())
		calc2<-1/(n-p)
		dispersion<-if((calc2*calc)>1) calc2*calc else 1  	#Final calculation of dispersion parameter
		U<-threshold(z,0)
	}
	
	if(expectedc<0.1) U<-1.9                 #threshold starts to increase if expected<0.1 - so fix at value << 2 => if get two cases then raise flag.
	if (currentCount==0) {
		expectedc<-0 
		U<-0 
	}
	thresh<-U
	exceed<-(currentCount-expectedc)/(U-expectedc)
	if (U==0) exceed<-0 else exceed<-exceed
	
	ifelse( (sum(basecont)==0 && currentCount>0),{exceed='>1'},{exceed=exceed})
	#----------transform back to original counts (add back in seasonality)-----------------
	currentc1=currentCount+tsSeasonal[length(tsSeasonal)] +tsRandom[length(tsRandom)]
	expectedc1=expectedc+tsSeasonal[length(tsSeasonal)] +tsRandom[length(tsRandom)]
	thresh1=thresh+tsSeasonal[length(tsSeasonal)] +tsRandom[length(tsRandom)]
	} 

#note if not taken out seasonality then nothing will change here.
#----output data --------------------------------------------------------------------
output = toJSON(list(
				"expected" = expectedc1[[1]],
				"threshold" = thresh1[[1]],
				"trend" = trend,
				"exceed" = exceed[[1]],
				"weights" = w
		))