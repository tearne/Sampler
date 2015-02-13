#homeDir<-"//home/user/ENVIRONMENT/workspaces/workspace_scala/FarringtonTest/"
#fileDir<-"EDS"
#outDir<-paste(homeDir, "results", sep="")
#setwd(paste(homeDir,fileDir,sep=""))

require(rjson)

#allData = as.data.frame(fromJSON(readLines("tmp.json")))

########Model can not cope with expected[i] being too small and crashes (produces #NAN's).
########As it rounds it off and effectively tries to divide by zero
########Therefore I have set expected[i] to be an extremely small number in the #event that it is effectively zero
########This will not effect the validity of the results 

notZero=1*10^(-150)

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

baselineNum = nrow(basedata)
#baseData = allData[1:baselineNum,]
#currentCount = allData[nrow(allData),"Incidents"]
#currentmth = allData[nrow(allData), "MonthNum"]
basemth = basedata$basemth
basecont = basedata$basecont

#basedata<-data.frame(basecont, basemth)
# currentc<-Data2[currentmth, 2+2]  	#LEAVE AS [,2+2] so only look at INCIDENTS. If wish to look at isolations change to [,1+1]. 
n=nrow(basedata)
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
m<-length(residuals1[residuals1<1])
k=rep(0,times=n)
	k[residuals1<1]=residuals1[residuals1<1]^-2
y<-n/(sum(k)+(n-m))				#Calculate the weights
w=k*y
	w[w==0]=y     
#----fit model with weights--------------------
modelW<-glm(formula=basecont~basemth, family=quasipoisson(link=log), weights=w, data=basedata)   
	paramW<-coef(modelW)
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
	trend="noTrend"
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

ifelse(sum(basecont)==0&&currentc>0,exceed<-'>1',exceed<-exceed)

output = toJSON(list(
				"expected" = expectedc[[1]],
				"threshold" = thresh[[1]],
				"trend" = trend,
				"exceed" = exceed[[1]],
				"weights" = w
		))