#homeDir<-"//home/user/ENVIRONMENT/workspaces/workspace_scala/FarringtonTest/"
#fileDir<-"EDS"
#outDir<-paste(homeDir, "results", sep="")
#setwd(paste(homeDir,fileDir,sep=""))

require(rjson)

#allData = as.data.frame(fromJSON(readLines("tmp.json")))

#######################################################
## DISPERSION 
## This function estimates the dispersion parameter using the formula described in Farrington #(1996)
#######################################################

disp<-function(i){
	calc<-numeric(baselineNum)
	i<-1
	while(i<baselineNum+1){
		ifelse(expected[i]<1*10^(-150), calc[i]<-1*10^(-150), calc[i]<-w[i]*(((basecont[i]-expected[i])^2)/expected[i]))
		i<-i+1
	}
	calc
}

########Model can not cope with expected[i] being too small and crashes (produces #NAN's).
########As it rounds it off and effectively tries to divide by zero
########Therefore I have set expected[i] to be an extremely small number in the #event that it is effectively zero
########THis will not effect the validity of the results 

###################################################
## DERIVE RESIDUALS	 
## This code is used to derive the anscombe residuals which are used as part of the re-weighting 
## procedure in accounting for past outbreaks. The formula is described in Farrington #(1996)
###################################################

residuals <- function(i){
	b<-numeric(baselineNum)
	i<-1
	model<-glm(formula=basecont~basemth, family=quasipoisson(link=log), weights=w, data=basedata)
	hat<-lm.influence(model)$hat
	while(i<baselineNum+1){     
		ifelse(expected[i]<1*10^(-150),b[i]<-1*10^(-150),b[i]<-(3/(2*(dispersion)^(1/2)))*(((basecont[i])^(2/3))-((expected[i])^(2/3)))/(((expected[i])^(1/6))*((1-hat[i])^(1/2))))
		i<-i+1
	}
	b
}

#########Model can not cope with expected[i] being too small and crashes (produces NAN's).
#########As it rounds it off and effectively tries to divide by zero
#########Therefore I have set expected[i] to be an extremely small number in the event that it is effectively zero
#########THis will not effect the validity of the results 

###################################################
## DERIVE WEIGHTS - PART 1	 
##
## This code is used to derive the first part of the weighting formula, namely to determine the #number
## of times that the residuals are <1. These are summed up in the main body of the algorithm. #Part 1 
## is used to calculate y which is then used in the final calculation of the weights.
###################################################


xconstant <- function(i){
	c<-numeric(baselineNum)
	i<-1
	while(i<baselineNum+1){
		c[i]<-if(residuals1[i]<1) 1 else 0
		i<-i+1
	}
	c
}

###################################################
## DERIVE WEIGHTS - PART 2	 
##
## This code is used to calculate the (residuals)^-2 when the residual(i) <1. These values are #summed up 
## in the main body of the algorithm. Part 2 is used to calculate y which is then used in the #final 
## calculation of the weights.
####################################################

constant1 <- function(i){
	c<-numeric(baselineNum)
	i<-1 
	while (i<baselineNum+1){
		c[i]<-if(residuals1[i]<1) ((residuals1[i]^-2)) else 0
		i<-i+1
	}
	c
}

###################################################
## DERIVE WEIGHTS - PART 3	 
##
## This code is used to derive the final weighting calculation. For each baseline data point, a #weight
## is calculated depending upon if the residuals are <1. 
###################################################

weights <-function(i){
	w2<-numeric(baselineNum)
	i<-1
	while(i<baselineNum+1){
		w2[i]<-if(residuals1[i]<1) (y*(residuals1[i]^-2)) else y
		i<-i+1
	}
	w2
}

###################################################
## DERIVE THRESHOLD VALUES	 
##
## This code is used to derive the threshold values using the formula described in Farrington #1996. 
## Two weights are derived -  "threshold" if the linear trend is included and "threshold2" if the
## the linear trend is not significant.
###################################################

#calculation if model is to include linear trend
threshold <-function(z){
	var<-varalpha+(currentmth*currentmth)*varbeta+(2*currentmth*covariance)
	tao<-(dispersion*expectedc+var)/(expectedc^2)
	U<-expectedc*(1+(2/3)*z*(tao^0.5))^(3/2)
	U
}

#calculation if not include linear trend
threshold2 <- function(z){
	var<-varalpha
	tao<-(dispersion*expectedc+var)/(expectedc^2)
	U<-expectedc*(1+(2/3)*z*(tao^0.5))^(3/2)
	U
}

###########################################################################
###				MAIN MODEL				
###########################################################################

#allData = read.csv("/home/user/ENVIRONMENT/workspaces/workspace_scala/FarringtonTest/results/baselineData.txt")
#allData = as.data.frame(fromJSON(paste(readLines("r-in.json"), collapse="")))

baselineNum = nrow(basedata)
#baseData = allData[1:baselineNum,]
#currentCount = allData[nrow(allData),"Incidents"]
#currentmth = allData[nrow(allData), "MonthNum"]
basemth = basedata$basemth
basecont = basedata$basecont

#basedata<-data.frame(basecont, basemth)
# currentc<-Data2[currentmth, 2+2]  	#LEAVE AS [,2+2] so only look at INCIDENTS. If wish to look at isolations change to [,1+1]. 

w<-rep(1,times=nrow(basedata))					#Set weights = 1 

#---calculate weights----------------
model<-glm(formula=basecont~basemth, family=quasipoisson(link=log), weights=w, data=basedata)	#Fit model
param<-coef(model)
expected<-exp(param[1]+(basemth*param[2]))		#Estimate expected values
expectedc<-exp(param[1]+(currentmth*param[2]))	#Estimate expected value for current month
coeff<-summary(model)$coeff
varalpha<-coeff[1,2]*coeff[1,2]
varbeta<-coeff[2,2]*coeff[2,2]
cova<-vcov(model)					#Covariance matrix
covariance<-cova[1,2]
p<-2						#Number of esitmated parameters
trend=1          #denotes trend is included
calc<-sum(disp())					#First calculation of dispersion parameter
calc2<-1/(baselineNum-p)
dispersion<-if((calc2*calc)>1) calc2*calc else 1		#Calculate dispersion parameter final
residuals1<-residuals()				#Estimate the residuals
m<-sum(xconstant())
k<-constant1()
y<-baselineNum/(sum(k)+(baselineNum-m))				#Calculate the weights
w<-weights()					#Obtain the weights vector for baseline data

#----refit model with weights--------------------
model<-glm(formula=basecont~basemth, family=quasipoisson(link=log), weights=w, data=basedata)
param<-coef(model)
expected<-exp(param[1]+(basemth*param[2]))		#Estimate expected values again
expectedc<-exp(param[1]+(currentmth*param[2]))	#Estimate expected value for current month again	
coeff<-summary(model)$coeff
varalpha<-coeff[1,2]*coeff[1,2]
varbeta<-coeff[2,2]*coeff[2,2]
cova<-vcov(model)					#Covariance matrix
covariance<-cova[1,2]
calc<-sum(disp())					#Calculate the first part of dispersion parameter again
calc2<-1/(baselineNum-p)
dispersion<-if((calc2*calc)>1) calc2*calc else 1		#Recalculate the dispersion parameter
tvalue<-coeff[2]/coeff[2,2]		#T-value for beta
df<-baselineNum-p-1						#Degrees freedom

ttest<-qt(0.95,df)					#T-test for the significance of linear trend
z<-1.96  
U<-threshold(z)		#Calculate the threshold 

if (!( (tvalue>ttest | tvalue < -ttest) & expectedc<=max(basecont)))  { 
	#----fit model with no linear trend-----------------------------
	model<-glm(formula=basecont~1, family=quasipoisson(link=log), weights=w, data=basedata)  #model with no linear trend
	expected<-expected*0+exp(coef(model))
	expectedc<-exp(coef(model))
	trend="noTrend"
	p<-1
	calc<-sum(disp())
	calc2<-1/(n-p)
	dispersion<-if((calc2*calc)>1) calc2*calc else 1  	#Final calculation of dispersion parameter
	U<-threshold2(z)
}

if(expectedc<0.1) U<-1.9                 #threshold starts to increase if expected<0.1 - so fix at value << 2 => if get two cases then raise flag.
if (currentCount==0) {
	expectedc<-0 
	U<-0 
}
thresh<-U
exceed<-(currentCount-expectedc)/(U-expectedc)
if (U==0) exceed<-0 else exceed<-exceed

ifelse(sum(basecont)==0&&currentc>0,exceed<-'>1',exceed<-exceed)

output = toJSON(list(
				"expected" = expectedc[[1]],
				"threshold" = thresh[[1]],
				"trend" = trend,
				"exceed" = exceed[[1]],
				"weights" = w
		))