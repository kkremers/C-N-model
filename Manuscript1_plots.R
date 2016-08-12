###########Analysis of Climate data for Results####################

data = read.csv("OriginalData_NOTfilled.csv") #choose file from directory
names(data) = c("time", "year", "DOY","Temp_T", "PAR_T", "Temp_ARF", "PAR_ARF")
head(data) #view table

#set working directory to C-N-model
data1 = read.csv("InputData_Processed.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data1)


data2 = data.frame(read.csv("VPD_Precip_Daily.csv"))
head(data2)
data2$VPD_Avg = as.numeric(as.character(data2$VPD_Avg))



#plot
par(mar=c(2,4,2,2), las=1, mfrow=c(2,1), oma=c(0,0,0,0))
plot(data1$Temp_ARF~data1$time, cex.axis = 1.5, type="l", ylim=c(-40,25), ylab = "", col="black", xlab="", axes=FALSE)
axis(1, at=c(0,365,730,1096,1461,1826,2191,2556,2922), labels=c("","","","","","","","",""))
mtext("2008", side=1, at=182, cex=2, line=0.8)
mtext("2009", side=1, at=547, cex=2, line=0.8) 
mtext("2010", side=1, at=912, cex=2, line=0.8) 
mtext("2011", side=1, at=1277, cex=2, line=0.8) 
mtext("2012", side=1, at=1642, cex=2, line=0.8) 
mtext("2013", side=1, at=2007, cex=2, line=0.8) 
mtext("2014", side=1, at=2372, cex=2, line=0.8)
mtext("2015", side=1, at=2737, cex=2, line=0.8) 
axis(2, cex.axis=1.5)
points(data$Temp_ARF~data$time, pch=16, col="gray40", cex=0.75)
abline(h=0, lty=2)
plot(data1$PAR_ARF~data1$time, cex.axis=1.5,type="l", axes=FALSE, ylab = "", col="black", xlab = "")
axis(1, at=c(0,365,730,1096,1461,1826,2191,2556,2922), labels=c("","","","","","","","",""))
mtext("2008", side=1, at=182, cex=2, line=0.8)
mtext("2009", side=1, at=547, cex=2, line=0.8) 
mtext("2010", side=1, at=912, cex=2, line=0.8) 
mtext("2011", side=1, at=1277, cex=2, line=0.8) 
mtext("2012", side=1, at=1642, cex=2, line=0.8) 
mtext("2013", side=1, at=2007, cex=2, line=0.8) 
mtext("2014", side=1, at=2372, cex=2, line=0.8)
mtext("2015", side=1, at=2737, cex=2, line=0.8) 
axis(2, cex.axis=1.5)
points(data$PAR_ARF*(1E-6)*86400~data$time, pch=16, col="gray40", cex=0.75)




#run "2_DataReload.R"

head(data)
head(data2)
#calculate values for table 3
years = unique(data$year) #tells you which years we have data for 
GSlength = NA
GS_Tavg = NA
OS_Tavg = NA
GS_PARavg = NA
GS_Precip = NA
GS_VPD = NA
Tmax=NA


for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  data2.year = subset(data2, data2$Year==year.i)
  GSlength[i] = data.year$endDOY[1] - data.year$startDOY[1]
  Temp_GS = data.year$Temp_ARF[data.year$DOY>=data.year$startDOY[1] & data.year$DOY <= data.year$endDOY[1]]
  Temp_OS = data.year$Temp_ARF[data.year$DOY<data.year$startDOY[1] | data.year$DOY > data.year$endDOY[1]]
  PAR_GS = data.year$PAR_ARF[data.year$DOY>=data.year$startDOY[1] & data.year$DOY <= data.year$endDOY[1]]
  Precip_GS = data2.year$Precip_Tot[data2.year$DOY>=data.year$startDOY[1] & data2.year$DOY <= data.year$endDOY[1]]
  VPD_GS = data2.year$Precip_Tot[data2.year$DOY>=data.year$startDOY[1] & data2.year$DOY <= data.year$endDOY[1]]
  
  GS_Tavg[i] = mean(Temp_GS)
  OS_Tavg[i] = mean(Temp_OS)
  GS_PARavg[i] = mean(PAR_GS)
  GS_Precip[i] = sum(Precip_GS, na.rm=TRUE)
  GS_VPD[i] = mean(VPD_GS, na.rm=TRUE)
  Tmax[i] = max(data.year$Temp_ARF)
}

GSlength
GS_Tavg
OS_Tavg
GS_PARavg
GS_Precip
GS_VPD
Tmax
years
assim = c(0,1,0,1,0,1,0,1) #0=NO, 1=YES
stats=data.frame(Year=years, Assim=assim, GSlength, GS_Tavg, OS_Tavg, GS_PARavg, GS_Precip, Tmax)
stats


#look at values for assimilated vs. not assimilated data

tapply(stats$GSlength, stats$Assim, mean)
tapply(stats$GS_Tavg, stats$Assim, mean)
tapply(stats$OS_Tavg, stats$Assim, mean)
tapply(stats$GS_PARavg, stats$Assim, mean)
tapply(stats$GS_Precip, stats$Assim, mean)
tapply(stats$Tmax, stats$Assim, mean)


GSlength.test = t.test(stats$GSlength[stats$Assim==0], stats$GSlength[stats$Assim==1])
GSlength.test
GS_Tavg.test = t.test(stats$GS_Tavg[stats$Assim==0], stats$GS_Tavg[stats$Assim==1])
GS_Tavg.test
OS_Tavg.test = t.test(stats$OS_Tavg[stats$Assim==0], stats$OS_Tavg[stats$Assim==1])
OS_Tavg.test
GS_PARavg.test = t.test(stats$GS_PARavg[stats$Assim==0], stats$GS_PARavg[stats$Assim==1])
GS_PARavg.test
GS_Precip.test = t.test(stats$GS_Precip[stats$Assim==0], stats$GS_Precip[stats$Assim==1])
GS_Precip.test
Tmax.test = t.test(stats$Tmax[stats$Assim==0], stats$Tmax[stats$Assim==1])
Tmax.test #this is the only one significanty different




#################NOW DO THE SAME FOR NEE AND NDVI DATA#################

data = read.csv("Assimilation_data_ALL.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data)

#calculate yearly max, min, and GS avg temp and PAR
years = unique(data$Year) #tells you which years we have data for 
NEEmin = NA
NEEavg = NA
NDVImax = NA
NDVIavg = NA

for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$Year==year.i)
  NEEmin[i]=min(data.year$NEE, na.rm=TRUE)
  NDVImax[i]=max(data.year$NDVI, na.rm=TRUE)
  NEE_GS = data.year$NEE[data.year$DOY>=150 & data.year$DOY <= 240]
  NEEavg[i] = mean(NEE_GS, na.rm=TRUE)
  NDVI_GS = data.year$NDVI[data.year$DOY>=150 & data.year$DOY <= 240]
  NDVIavg[i] = mean(NDVI_GS, na.rm=TRUE)  
}

#Determine DOY of peak NEE and peak NDVI
NEE.day = NA
NDVI.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  NEE.i = NEEmin[i]
  NDVI.i = NDVImax[i]
  data.year = subset(data, data$Year==year.i)
  NEE.day[i] = data.year$DOY[which(data.year$NEE==NEE.i)]
  NDVI.day[i] = data.year$DOY[which(data.year$NDVI==NDVI.i)]
}


#look at data for all years
assim = c(0,1,0,1,0,1,0,1) #0=NO, 1=YES
stats1=data.frame(Year=years, Assim=assim, NEEmin, NDVImax, NEEavg, NDVIavg, NEE.day, NDVI.day)
stats1

#stats for NEEmin
min(NEEmin); max(NEEmin); mean(NEEmin); sd(NEEmin)
#stats for NDVImax
min(NDVImax); max(NDVImax); mean(NDVImax); sd(NDVImax)
#stats for NEEavg
min(NEEavg); max(NEEavg); mean(NEEavg); sd(NEEavg)
#stats for NDVIavg
min(NDVIavg); max(NDVIavg); mean(NDVIavg); sd(NDVIavg)
#stats for NEE.day
min(NEE.day); max(NEE.day); mean(NEE.day); sd(NEE.day)
#stats for NDVI.day
min(NDVI.day); max(NDVI.day); mean(NDVI.day); sd(NDVI.day)

#now look at values for assimilated vs. not assimilated data

tapply(stats1$NEEmin, stats1$Assim, mean)
tapply(stats1$NDVImax, stats1$Assim, mean)
tapply(stats1$NEEavg, stats1$Assim, mean)
tapply(stats1$NDVIavg, stats1$Assim, mean)
tapply(stats1$NEE.day, stats1$Assim, mean)
tapply(stats1$NDVI.day, stats1$Assim, mean)

NEEmin.test = t.test(stats1$NEEmin[stats1$Assim==0], stats1$NEEmin[stats1$Assim==1])
NEEmin.test
NDVImax.test = t.test(stats1$NDVImax[stats1$Assim==0], stats1$NDVImax[stats1$Assim==1])
NDVImax.test
NEEavg.test = t.test(stats1$NEEavg[stats1$Assim==0], stats1$NEEavg[stats1$Assim==1])
NEEavg.test
NDVIavg.test = t.test(stats1$NDVIavg[stats1$Assim==0], stats1$NDVIavg[stats1$Assim==1])
NDVIavg.test
NEE.day.test = t.test(stats1$NEE.day[stats1$Assim==0], stats1$NEE.day[stats1$Assim==1])
NEE.day.test
NDVI.day.test = t.test(stats1$NDVI.day[stats1$Assim==0], stats1$NDVI.day[stats1$Assim==1])
NDVI.day.test



#####################PLOTS FOR MANUSCRIPT 1######################

#load workspace from step 2

##################Histograms########################
par(mfrow=c(4,2), mar=c(5,6,2,2), las=0)
plot(density(param.keep[,1]), main="", ylab="Density", xlab=names(params[1]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[1], col="red", lwd=3)
plot(density(param.keep[,2]), main="", ylab="Density", xlab=names(params[2]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[2], col="red", lwd=3)
plot(density(param.keep[,3]), main="", ylab="Density", xlab=names(params[3]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[3], col="red", lwd=3)
plot(density(param.keep[,4]), main="", ylab="Density", xlab=names(params[4]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[4], col="red", lwd=3)
plot(density(param.keep[,5]), main="", ylab="Density", xlab=names(params[5]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[5], col="red", lwd=3)
plot(density(param.keep[,6]), main="", ylab="Density", xlab=names(params[6]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[6], col="red", lwd=3)
plot(density(param.keep[,7]), main="", ylab="Density", xlab=names(params[7]), cex.lab=2, cex.axis = 1.5)
abline(v=param.best[7], col="red", lwd=3)

param.min
param.max

param.priors = data.frame(matrix(1, 1000,7))
colnames(param.priors)=names(param.keep)
head(param.priors)
param.priors[,1] = runif(1000, param.min[1], param.max[1])
param.priors[,2] = runif(1000, param.min[2], param.max[2])
param.priors[,3] = runif(1000, param.min[3], param.max[3])
param.priors[,4] = runif(1000, param.min[4], param.max[4])
param.priors[,5] = runif(1000, param.min[5], param.max[5])
param.priors[,6] = runif(1000, param.min[6], param.max[6])
param.priors[,7] = runif(1000, param.min[7], param.max[7])

param.25_old = apply(param.priors, 2, quantile, 0.25)
param.75_old = apply(param.priors, 2, quantile, 0.75)
param.IQR_old = param.75_old-param.25_old

param.25_new = apply(param.keep, 2, quantile, 0.25)
param.75_new = apply(param.keep, 2, quantile, 0.75)
param.IQR_new = param.75_new-param.25_new

param.IQR_new/param.IQR_old


param.min_new = apply(param.keep, 2, min)
param.max_new = apply(param.keep, 2, max)
apply(param.keep,2,mean)
apply(param.keep,2,sd)

range.i = param.max-param.min
range.f = param.max_new - param.min_new

range.f<range.i
mean(range.f/range.i)
range.f/range.i


###################Temporal Validation#################

time = seq(1:length(data$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)

out= data.frame(solvemodel(param.best, state.best)) #creates table of model output


#load data
data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = data.assim[data.assim$Year == c(2009,2011,2013,2015),]
data.sigma = data.sigma[data.sigma$Year  == c(2009,2011,2013,2015),]
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,7]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,7]))
head(data.compare1)
head(sigma.obs1)


#run model code that does NOT include starting values as params
require(FME)

q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
summarytable=data.frame(q05 = q05, q25 = q25, mean = means, 
                        q75 = q75, q95 = q95) #bind all of the information together in the proper order (same order as summarytable columns)


#global sensitivity analysis to find confidence intervals

params.all = cbind(param.keep, state.keep)
head(params.all)

sensvars = c("NEE",
             "NDVI")

params.sens = c(param.best, state.best)
params.sens

#run 3_Model_startingvaluesasparams.R
require(FME)
s.global <- sensRange(func=solvemodel1, parms=params.sens, sensvar = sensvars, parInput=params.all)
s.global.summ = summary(s.global) #create summary table
head(s.global.summ) #view first 6 rows
tail(s.global.summ)

#get model output & confidence intervals organized
out=data.frame(solvemodel(param.best, state.best))
NEE_summ = data.frame(Time=s.global.summ[1:2922,1], NEE=out$NEE, sd=s.global.summ[1:2922,3], min=s.global.summ[1:2922,4], max=s.global.summ[1:2922,5], q05=s.global.summ[1:2922,6], q95=s.global.summ[1:2922,10], q25=s.global.summ[1:2922,7], q75=s.global.summ[1:2922,9])
head(NEE_summ)
NDVI_summ = data.frame(Time=s.global.summ[2923:5844,1], NDVI=out$NDVI, sd=s.global.summ[2923:5844,3], min=s.global.summ[2923:5844,4], max=s.global.summ[2923:5844,5],q05=s.global.summ[2923:5844,6], q95=s.global.summ[2923:5844,10], q25=s.global.summ[2923:5844,7], q75=s.global.summ[2923:5844,9])
head(NDVI_summ)

#get data ready
data.compare=read.csv("Assimilation_data_ALL.csv")
sigma.compare=read.csv("Assimilation_sigma_ALL.csv")
data.compare1=subset(data.compare, Year== 2009 | Year== 2011 | Year== 2013 | Year== 2015)
data.compare2=subset(data.compare, Year== 2008 | Year== 2010 | Year== 2012 | Year== 2014)
data.compare_NEE1=data.compare1[complete.cases(data.compare1[,6]),c(1:5,6)]
data.compare_NEE2=data.compare2[complete.cases(data.compare2[,6]),c(1:5,6)]
data.compare_NDVI1=data.compare1[complete.cases(data.compare1[,7]),c(1:5,7)]
data.compare_NDVI2=data.compare2[complete.cases(data.compare2[,7]),c(1:5,7)]
out.compare_NEE1 = out[match(data.compare_NEE1$Time, out$time),]
out.compare_NEE2 = out[match(data.compare_NEE2$Time, out$time),]
out.compare_NDVI1 = out[match(data.compare_NDVI1$Time, out$time),]
out.compare_NDVI2 = out[match(data.compare_NDVI2$Time, out$time),]
data.compare_NEE=data.compare[complete.cases(data.compare[,6]),c(1:5,6)]
data.compare_NDVI=data.compare[complete.cases(data.compare[,7]),c(1:5,7)]
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]
VPD.compare_NEE = data2[match(data.compare_NEE$Time, data2$Time),]
VPD.compare_NDVI = data2[match(data.compare_NDVI$Time, data2$Time),]
sigma.compare_NEE = sigma.compare[match(data.compare_NEE$Time, sigma.compare$Time),]
sigma.compare_NDVI = sigma.compare[match(data.compare_NDVI$Time, sigma.compare$Time),]



#linear regressions for years not assimilated
reg_NEE = lm(out.compare_NEE2$NEE~data.compare_NEE2$NEE)
reg_NDVI = lm(out.compare_NDVI2$NDVI~data.compare_NDVI2$NDVI)

summary(reg_NEE)
summary(reg_NDVI)




#now need to calculate residuals and determine RMSE for each year
resid.NEE2 = data.compare_NEE2$NEE - out.compare_NEE2$NEE
resid.NEE1 = data.compare_NEE1$NEE - out.compare_NEE1$NEE
resid.NEE = c(resid.NEE1, resid.NEE2)
resid_NEE = data.frame(time=c(out.compare_NEE1$time, out.compare_NEE2$time), Year=c(out.compare_NEE1$year, out.compare_NEE2$year), DOY=c(out.compare_NEE1$DOY, out.compare_NEE2$DOY), Temp = c(out.compare_NEE1$Temp, out.compare_NEE2$Temp), PAR = c(out.compare_NEE1$PAR, out.compare_NEE2$PAR), VPD = VPD.compare_NEE$VPD_Avg, resid.NEE, sigma=sigma.compare_NEE$NEE)
resid_NEE = resid_NEE[ order(resid_NEE[,2]), ]
head(resid_NEE)

resid.NDVI2 = data.compare_NDVI2$NDVI - out.compare_NDVI2$NDVI
resid.NDVI1 = data.compare_NDVI1$NDVI - out.compare_NDVI1$NDVI
resid.NDVI = c(resid.NDVI1, resid.NDVI2)
resid_NDVI = data.frame(time=c(out.compare_NDVI1$time, out.compare_NDVI2$time), Year=c(out.compare_NDVI1$year, out.compare_NDVI2$year), DOY=c(out.compare_NDVI1$DOY, out.compare_NDVI2$DOY), Temp = c(out.compare_NDVI1$Temp, out.compare_NDVI2$Temp), PAR = c(out.compare_NDVI1$PAR, out.compare_NDVI2$PAR), VPD = VPD.compare_NDVI$VPD_Avg, resid.NDVI, sigma=sigma.compare_NDVI$NDVI)
resid_NDVI = resid_NDVI[ order(resid_NDVI[,2]), ]
head(resid_NDVI)

#NEED TO CALCULATE MOVING AVERAGE FOR THESE PLOTS
#DOY
resid_NEE = resid_NEE[order(resid_NEE$DOY),] #put it in order
#now calculate smoothed means of residuals
residNEE.DOY.smooth = tapply(resid_NEE$resid.NEE, (cut(resid_NEE$DOY, seq(min(resid_NEE$DOY), max(resid_NEE$DOY), by=6))), mean)
sigmaNEE.DOY.smooth = tapply(resid_NEE$sigma, (cut(resid_NEE$DOY, seq(min(resid_NEE$DOY), max(resid_NEE$DOY), by=6))), mean)
timeNEE.DOY.smooth=seq(152, length=length(residNEE.DOY.smooth), by=6) #create corresponding time series

resid_NDVI = resid_NDVI[order(resid_NDVI$DOY),] #put it in order
#now calculate smoothed means of residuals
residNDVI.DOY.smooth = tapply(resid_NDVI$resid.NDVI, (cut(resid_NDVI$DOY, seq(min(resid_NDVI$DOY), max(resid_NDVI$DOY), by=6))), mean)
sigmaNDVI.DOY.smooth = tapply(resid_NDVI$sigma, (cut(resid_NDVI$DOY, seq(min(resid_NDVI$DOY), max(resid_NDVI$DOY), by=6))), mean)
timeNDVI.DOY.smooth=seq(152, length=length(residNDVI.DOY.smooth), by=6) #create corresponding time series

#TEMP
resid_NEE = resid_NEE[order(resid_NEE$Temp),] #put it in order
#now calculate smoothed means of residuals
residNEE.Temp.smooth = tapply(resid_NEE$resid.NEE, (cut(resid_NEE$Temp, seq(min(resid_NEE$Temp), max(resid_NEE$Temp), by=2))), mean)
sigmaNEE.Temp.smooth = tapply(resid_NEE$sigma, (cut(resid_NEE$Temp, seq(min(resid_NEE$Temp), max(resid_NEE$Temp), by=2))), mean)
timeNEE.Temp.smooth=seq(0.17, length=length(residNEE.Temp.smooth), by=2) #create corresponding time series

resid_NDVI = resid_NDVI[order(resid_NDVI$Temp),] #put it in order
#now calculate smoothed means of residuals
residNDVI.Temp.smooth = tapply(resid_NDVI$resid.NDVI, (cut(resid_NDVI$Temp, seq(min(resid_NDVI$Temp), max(resid_NDVI$Temp), by=2))), mean)
sigmaNDVI.Temp.smooth = tapply(resid_NDVI$sigma, (cut(resid_NDVI$Temp, seq(min(resid_NDVI$Temp), max(resid_NDVI$Temp), by=2))), mean)
timeNDVI.Temp.smooth=seq(0.17, length=length(residNDVI.Temp.smooth), by=2) #create corresponding time series

#PAR
resid_NEE = resid_NEE[order(resid_NEE$PAR),] #put it in order
#now calculate smoothed means of residuals
residNEE.PAR.smooth = tapply(resid_NEE$resid.NEE, (cut(resid_NEE$PAR, seq(min(resid_NEE$PAR), max(resid_NEE$PAR), by=3))), mean)
sigmaNEE.PAR.smooth = tapply(resid_NEE$sigma, (cut(resid_NEE$PAR, seq(min(resid_NEE$PAR), max(resid_NEE$PAR), by=3))), mean)
timeNEE.PAR.smooth=seq(5.42, length=length(residNEE.PAR.smooth), by=3) #create corresponding time series

resid_NDVI = resid_NDVI[order(resid_NDVI$PAR),] #put it in order
#now calculate smoothed means of residuals
residNDVI.PAR.smooth = tapply(resid_NDVI$resid.NDVI, (cut(resid_NDVI$PAR, seq(min(resid_NDVI$PAR), max(resid_NDVI$PAR), by=3))), mean)
sigmaNDVI.PAR.smooth = tapply(resid_NDVI$sigma, (cut(resid_NDVI$PAR, seq(min(resid_NDVI$PAR), max(resid_NDVI$PAR), by=3))), mean)
timeNDVI.PAR.smooth=seq(5.42, length=length(residNDVI.PAR.smooth), by=3) #create corresponding time series

#VPD
resid_NEE = resid_NEE[order(resid_NEE$VPD),] #put it in order
#now calculate smoothed means of residuals
residNEE.VPD.smooth = tapply(resid_NEE$resid.NEE, (cut(resid_NEE$VPD, seq(min(resid_NEE$VPD), max(resid_NEE$VPD), by=0.1))), mean)
sigmaNEE.VPD.smooth = tapply(resid_NEE$sigma, (cut(resid_NEE$VPD, seq(min(resid_NEE$VPD), max(resid_NEE$VPD), by=0.1))), mean)
timeNEE.VPD.smooth=seq(0.06, length=length(residNEE.VPD.smooth), by=0.1) #create corresponding time series

resid_NDVI = resid_NDVI[order(resid_NDVI$VPD),] #put it in order
#now calculate smoothed means of residuals
residNDVI.VPD.smooth = tapply(resid_NDVI$resid.NDVI, (cut(resid_NDVI$VPD, seq(min(resid_NDVI$VPD), max(resid_NDVI$VPD), by=0.1))), mean)
sigmaNDVI.VPD.smooth = tapply(resid_NDVI$sigma, (cut(resid_NDVI$VPD, seq(min(resid_NDVI$VPD), max(resid_NDVI$VPD), by=0.1))), mean)
timeNDVI.VPD.smooth=seq(0.06, length=length(residNDVI.VPD.smooth), by=0.1) #create corresponding time series


par(mfrow=c(4,2), mar=c(4,4,2,2), oma=c(0,0,0,0), las=0)
plot(resid_NEE$DOY, resid_NEE$resid.NEE, main="", ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNEE.DOY.smooth~timeNEE.DOY.smooth, col="white", lwd=7)
lines(sigmaNEE.DOY.smooth~timeNEE.DOY.smooth, col="gray50", lwd=3)
lines((-1*sigmaNEE.DOY.smooth)~timeNEE.DOY.smooth, col="white", lwd=7)
lines((-1*sigmaNEE.DOY.smooth)~timeNEE.DOY.smooth, col="gray50", lwd=3)
lines(residNEE.DOY.smooth~timeNEE.DOY.smooth, col="white", lwd=7)
lines(residNEE.DOY.smooth~timeNEE.DOY.smooth, col="red", lwd=3)


plot(resid_NDVI$DOY, resid_NDVI$resid.NDVI, main="", ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNDVI.DOY.smooth~timeNDVI.DOY.smooth, col="white", lwd=7)
lines(sigmaNDVI.DOY.smooth~timeNDVI.DOY.smooth, col="gray50", lwd=3)
lines((-1*sigmaNDVI.DOY.smooth)~timeNDVI.DOY.smooth, col="white", lwd=7)
lines((-1*sigmaNDVI.DOY.smooth)~timeNDVI.DOY.smooth, col="gray50", lwd=3)
lines(residNDVI.DOY.smooth~timeNDVI.DOY.smooth, col="white", lwd=7)
lines(residNDVI.DOY.smooth~timeNDVI.DOY.smooth, col="red", lwd=3)


plot(resid_NEE$Temp, resid_NEE$resid.NEE,  ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNEE.Temp.smooth~timeNEE.Temp.smooth, col="white", lwd=7)
lines(sigmaNEE.Temp.smooth~timeNEE.Temp.smooth, col="gray50", lwd=3)
lines((-1*sigmaNEE.Temp.smooth)~timeNEE.Temp.smooth, col="white", lwd=7)
lines((-1*sigmaNEE.Temp.smooth)~timeNEE.Temp.smooth, col="gray50", lwd=3)
lines(residNEE.Temp.smooth~timeNEE.Temp.smooth, col="white", lwd=7)
lines(residNEE.Temp.smooth~timeNEE.Temp.smooth, col="red", lwd=3)


plot(resid_NDVI$Temp, resid_NDVI$resid.NDVI, ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNDVI.Temp.smooth~timeNDVI.Temp.smooth, col="white", lwd=7)
lines(sigmaNDVI.Temp.smooth~timeNDVI.Temp.smooth, col="gray50", lwd=3)
lines((-1*sigmaNDVI.Temp.smooth)~timeNDVI.Temp.smooth, col="white", lwd=7)
lines((-1*sigmaNDVI.Temp.smooth)~timeNDVI.Temp.smooth, col="gray50", lwd=3)
lines(residNDVI.Temp.smooth~timeNDVI.Temp.smooth, col="white", lwd=7)
lines(residNDVI.Temp.smooth~timeNDVI.Temp.smooth, col="red", lwd=3)


plot(resid_NEE$PAR, resid_NEE$resid.NEE,  ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNEE.PAR.smooth~timeNEE.PAR.smooth, col="white", lwd=7)
lines((-1*sigmaNEE.PAR.smooth)~timeNEE.PAR.smooth, col="white", lwd=7)
lines(residNEE.PAR.smooth~timeNEE.PAR.smooth, col="white", lwd=7)
lines(sigmaNEE.PAR.smooth~timeNEE.PAR.smooth, col="gray50", lwd=3)
lines((-1*sigmaNEE.PAR.smooth)~timeNEE.PAR.smooth, col="gray50", lwd=3)
lines(residNEE.PAR.smooth~timeNEE.PAR.smooth, col="red", lwd=3)


plot(resid_NDVI$PAR, resid_NDVI$resid.NDVI, ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNDVI.PAR.smooth~timeNDVI.PAR.smooth, col="white", lwd=7)
lines((-1*sigmaNDVI.PAR.smooth)~timeNDVI.PAR.smooth, col="white", lwd=7)
lines(residNDVI.PAR.smooth~timeNDVI.PAR.smooth, col="white", lwd=7)
lines(sigmaNDVI.PAR.smooth~timeNDVI.PAR.smooth, col="gray50", lwd=3)
lines((-1*sigmaNDVI.PAR.smooth)~timeNDVI.PAR.smooth, col="gray50", lwd=3)
lines(residNDVI.PAR.smooth~timeNDVI.PAR.smooth, col="red", lwd=3)


plot(resid_NEE$VPD, resid_NEE$resid.NEE,  ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNEE.VPD.smooth~timeNEE.VPD.smooth, col="white", lwd=7)
lines((-1*sigmaNEE.VPD.smooth)~timeNEE.VPD.smooth, col="white", lwd=7)
lines(residNEE.VPD.smooth~timeNEE.VPD.smooth, col="white", lwd=7)
lines(sigmaNEE.VPD.smooth~timeNEE.VPD.smooth, col="gray50", lwd=3)
lines((-1*sigmaNEE.VPD.smooth)~timeNEE.VPD.smooth, col="gray50", lwd=3)
lines(residNEE.VPD.smooth~timeNEE.VPD.smooth, col="red", lwd=3)


plot(resid_NDVI$VPD, resid_NDVI$resid.NDVI, ylab="", xlab="", pch=16, cex.axis=2)
abline(h=0, lwd=2)
lines(sigmaNDVI.VPD.smooth~timeNDVI.VPD.smooth, col="white", lwd=7)
lines((-1*sigmaNDVI.VPD.smooth)~timeNDVI.VPD.smooth, col="white", lwd=7)
lines(residNDVI.VPD.smooth~timeNDVI.VPD.smooth, col="white", lwd=7)
lines(sigmaNDVI.VPD.smooth~timeNDVI.VPD.smooth, col="gray50", lwd=3)
lines((-1*sigmaNDVI.VPD.smooth)~timeNDVI.VPD.smooth, col="gray50", lwd=3)
lines(residNDVI.VPD.smooth~timeNDVI.VPD.smooth, col="red", lwd=3)



#calculate RMSE by year:
rmse <- function(x){
  sqrt(mean(x^2))
}
NEE_yrRMSE = tapply(resid_NEE$resid.NEE, resid_NEE$Year, rmse)
NDVI_yrRMSE = tapply(resid_NDVI$resid.NDVI, resid_NDVI$Year, rmse)


#calculate overall RMSE
RMSE.NEE = rmse(resid.NEE)
RMSE.NDVI = rmse(resid.NDVI)

#RMSE for years NOT assimilated
RMSE.NEE2 = rmse(resid.NEE2)
RMSE.NDVI2 = rmse(resid.NDVI2)


#plot
par(mar=c(5,5,1,1), las=1)
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(3,1.25))

plot(NEE~time,data=out, type="p", ylab="", pch=16, cex=1.5, axes=FALSE, xlab="", ylim=c(-5,6), cex.lab=1.5, col="white")
axis(1, at=c(0,365,730,1096,1461,1826,2191,2556,2922), labels=c("","","","","","","","",""))
mtext("2008", side=1, at=182, cex=1.5, line=0.8)
mtext("2009", side=1, at=547, cex=1.5, line=0.8) 
mtext("2010", side=1, at=912, cex=1.5, line=0.8) 
mtext("2011", side=1, at=1277, cex=1.5, line=0.8) 
mtext("2012", side=1, at=1642, cex=1.5, line=0.8) 
mtext("2013", side=1, at=2007, cex=1.5, line=0.8) 
mtext("2014", side=1, at=2372, cex=1.5, line=0.8)
mtext("2015", side=1, at=2737, cex=1.5, line=0.8) 
axis(2, cex.axis=2)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
#arrows(data.compare$Time,data.compare$NEE+sigma.compare$NEE, data.compare$Time, data.compare$NEE-sigma.compare$NEE, angle=90, code=3, length=0.01, col="gray50")
with(NEE_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = adjustcolor("gray60",alpha.f=1), border = adjustcolor("gray60",alpha.f=1)))
lines(NEE~time, data=out, lwd=1, col="black")
points(NEE~Time, data=data.compare_NEE1, pch=16, col="darkcyan", cex=0.5)
points(NEE~Time, data=data.compare_NEE2, pch=16, col="darkgoldenrod2", cex=0.5)
legend(1,5, cex=1.25, legend=c("CCaN NEE 90% C.I.", "CCaN NEE Estimate", "Assimilated NEE Measurements", "NEE Measurements Not Assimilated"), bty="n", pch=c(15,NA,16,16), lty=c(NA,1,NA,NA), col=c("gray60", "black", "darkcyan", "darkgoldenrod2"))

par(las=3)
barplot(NEE_yrRMSE, cex.names = 1.75, ylim=c(0,max(NEE_yrRMSE)+0.2), ylab="RMSE", cex=1.5, cex.axis=2, cex.lab = 2, col=c("darkgoldenrod2", "darkcyan", "darkgoldenrod2", "darkcyan", "darkgoldenrod2", "darkcyan", "darkgoldenrod2", "darkcyan"))

par(las=0)
plot(NDVI~time,data=out, type="p", pch=16, cex=1.5, axes=FALSE, xlab="",ylim=c(0,1), cex.lab=1.5, col="white", ylab="")
axis(1, at=c(0,365,730,1096,1461,1826,2191,2556,2922), labels=c("","","","","","","","",""))
mtext("2008", side=1, at=182, cex=1.5, line=0.8)
mtext("2009", side=1, at=547, cex=1.5, line=0.8) 
mtext("2010", side=1, at=912, cex=1.5, line=0.8) 
mtext("2011", side=1, at=1277, cex=1.5, line=0.8) 
mtext("2012", side=1, at=1642, cex=1.5, line=0.8) 
mtext("2013", side=1, at=2007, cex=1.5, line=0.8) 
mtext("2014", side=1, at=2372, cex=1.5, line=0.8)
mtext("2015", side=1, at=2737, cex=1.5, line=0.8) 
axis(2, cex.axis=2)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
#arrows(data.compare$Time,data.compare$NDVI+sigma.compare$NDVI, data.compare$Time, data.compare$NDVI-sigma.compare$NDVI, angle=90, code=3, length=0.01, col="gray50")
with(NDVI_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = adjustcolor("gray60",alpha.f=1), border = adjustcolor("gray60",alpha.f=1)))
lines(NDVI~time, data=out, pch=16, cex=0.75)
points(NDVI~Time, data=data.compare_NDVI1, pch=16, col="darkcyan", cex=0.5)
points(NDVI~Time, data=data.compare_NDVI2, pch=16, col="darkgoldenrod2", cex=0.5)
legend(1,0.95, cex=1.25, legend=c("CCaN NDVI 90% C.I.", "CCaN NDVI Estiamte", "Assimilated NDVI Measurements", "NDVI Measurements Not Assimilated"), bty="n", pch=c(15,NA,16,16), lty=c(NA,1,NA,NA), col=c("gray60", "black", "darkcyan", "darkgoldenrod2"))
par(las=3)
barplot(NDVI_yrRMSE, cex.names = 1.75, ylim=c(0,max(NDVI_yrRMSE)+0.01), ylab="RMSE", cex=1.5, cex.axis=2, cex.lab=2, col=c("darkgoldenrod2", "darkcyan", "darkgoldenrod2", "darkcyan", "darkgoldenrod2", "darkcyan", "darkgoldenrod2", "darkcyan"))

par(fig=c(0.45, 0.65, 0.75, 0.99), new = T, las=1)
plot(out.compare_NEE2$NEE,data.compare_NEE2$NEE, pch=16, cex.lab=1.25, cex=0.75, ylab="Measured NEE", xlab="",ylim=c(-4,2), xlim=c(-4,2), axes=FALSE)
axis(1, cex.axis = 1.25, at=c(-4,2), labels=c(-4,2))
axis(2, cex.axis = 1.25, at=c(-4,2), labels=c(-4,2))
abline(0,1, col="gray50", lty=2, lwd=2)
title(xlab="CCaN NEE", cex.lab = 1.25, line = 2)

par(fig=c(0.45, 0.65, 0.3, 0.54), new = T, las=1)
plot(out.compare_NDVI2$NDVI,data.compare_NDVI2$NDVI, axes=FALSE, pch=16, cex.lab=1.25, cex=0.75, ylab="Measured NDVI", xlab="", ylim=c(0.2,0.7), xlim=c(0.2,0.7))
axis(1, cex.axis = 1.25, at=c(0.2,0.7), labels=c(0.2,0.7))
axis(2, cex.axis = 1.25, at=c(0.2,0.7), labels=c(0.2,0.7))
abline(0,1, col="gray50", lty=2, lwd=2)
title(xlab="CCaN NDVI", cex.lab = 1.25, line = 2)


#want to calculate the proportion of measurements that fall inside model CI
head(NEE_summ)
head(data.compare_NEE)
head(sigma.compare_NEE)
NEE.check = NEE_summ[match(data.compare_NEE$Time, NEE_summ$Time),]
head(NEE.check)
NEE.check=cbind(NEE.check, NEEmeas = data.compare_NEE$NEE, NEEsigma = sigma.compare_NEE$NEE)
test1 = (NEE.check$NEEmeas-NEE.check$NEEsigma)>=NEE.check$q05 & (NEE.check$NEEmeas-NEE.check$NEEsigma)<=NEE.check$q95
test2 = (NEE.check$NEEmeas+NEE.check$NEEsigma)>=NEE.check$q05 & (NEE.check$NEEmeas+NEE.check$NEEsigma)<=NEE.check$q95
yes=rep(0, length(NEE.check[,1]))
for(i in 1:length(yes)){
  if(test1[i]==TRUE | test2[i]==TRUE){
    yes[i]=1
  }
}
yes
sum(yes)/length(yes) #76.6%

head(NDVI_summ)
head(data.compare_NDVI)
head(sigma.compare_NDVI)
NDVI.check = NDVI_summ[match(data.compare_NDVI$Time, NDVI_summ$Time),]
head(NDVI.check)
NDVI.check=cbind(NDVI.check, NDVImeas = data.compare_NDVI$NDVI, NDVIsigma = sigma.compare_NDVI$NDVI)
test1 = (NDVI.check$NDVImeas-NDVI.check$NDVIsigma)>=NDVI.check$q05 & (NDVI.check$NDVImeas-NDVI.check$NDVIsigma)<=NDVI.check$q95
test2 = (NDVI.check$NDVImeas+NDVI.check$NDVIsigma)>=NDVI.check$q05 & (NDVI.check$NDVImeas+NDVI.check$NDVIsigma)<=NDVI.check$q95
yes=rep(0, length(NDVI.check[,1]))
for(i in 1:length(yes)){
  if(test1[i]==TRUE | test2[i]==TRUE){
    yes[i]=1
  }
}
yes
sum(yes)/length(yes) #84.6%

###ALSO MAKE A PLOT OF HOW RMSE and R2 VARIES BY TIMESTEP###

#RMSE for daily timestep
#calculate overall RMSE
RMSE_NEE = rmse(resid.NEE)
RMSE_NDVI = rmse(resid.NDVI)
R2_NEE = cor(out.compare_NEE$NEE, data.compare_NEE$NEE)^2
R2_NDVI = cor(out.compare_NDVI$NDVI, data.compare_NDVI$NDVI)^2

#calculate 8 day average modelled and measured
nr = length(out.compare_NEE$NEE)
gr = rep(1:floor(nr/8), each = 8)
mod8day_NEE = aggregate(out.compare_NEE$NEE[-c(1:3)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NEE$NEE)
gr = rep(1:floor(nr/8), each = 8)
dat8day_NEE = aggregate(data.compare_NEE$NEE[-c(1:3)] ~ gr, FUN=mean)[,-1]

nr = length(out.compare_NDVI$NDVI)
gr = rep(1:floor(nr/8), each = 8)
mod8day_NDVI = aggregate(out.compare_NDVI$NDVI[-c(1:5)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NDVI$NDVI)
gr = rep(1:floor(nr/8), each = 8)
dat8day_NDVI = aggregate(data.compare_NDVI$NDVI[-c(1:5)] ~ gr, FUN=mean)[,-1]

RMSE_8dayNEE = rmse(dat8day_NEE-mod8day_NEE)
RMSE_8dayNDVI = rmse(dat8day_NDVI-mod8day_NDVI)
R2_8dayNEE = cor(dat8day_NEE,mod8day_NEE)^2
R2_8dayNDVI = cor(dat8day_NDVI,mod8day_NDVI)^2

#calculate 14 day average modelled and measured
nr = length(out.compare_NEE$NEE)
gr = rep(1:floor(nr/14), each = 14)
mod14day_NEE = aggregate(out.compare_NEE$NEE[-c(1:9)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NEE$NEE)
gr = rep(1:floor(nr/14), each = 14)
dat14day_NEE = aggregate(data.compare_NEE$NEE[-c(1:9)] ~ gr, FUN=mean)[,-1]

nr = length(out.compare_NDVI$NDVI)
gr = rep(1:floor(nr/14), each = 14)
mod14day_NDVI = aggregate(out.compare_NDVI$NDVI[-c(1:11)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NDVI$NDVI)
gr = rep(1:floor(nr/14), each = 14)
dat14day_NDVI = aggregate(data.compare_NDVI$NDVI[-c(1:11)] ~ gr, FUN=mean)[,-1]


RMSE_14dayNEE = rmse(dat14day_NEE-mod14day_NEE)
RMSE_14dayNDVI = rmse(dat14day_NDVI-mod14day_NDVI)
R2_14dayNEE = cor(dat14day_NEE,mod14day_NEE)^2
R2_14dayNDVI = cor(dat14day_NDVI,mod14day_NDVI)^2



#calculate 30 day average modelled and measured
nr = length(out.compare_NEE$NEE)
gr = rep(1:floor(nr/30), each = 30)
mod30day_NEE = aggregate(out.compare_NEE$NEE[-c(1:7)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NEE$NEE)
gr = rep(1:floor(nr/30), each = 30)
dat30day_NEE = aggregate(data.compare_NEE$NEE[-c(1:7)] ~ gr, FUN=mean)[,-1]

nr = length(out.compare_NDVI$NDVI)
gr = rep(1:floor(nr/30), each = 30)
mod30day_NDVI = aggregate(out.compare_NDVI$NDVI[-c(1:9)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NDVI$NDVI)
gr = rep(1:floor(nr/30), each = 30)
dat30day_NDVI = aggregate(data.compare_NDVI$NDVI[-c(1:9)] ~ gr, FUN=mean)[,-1]


RMSE_30dayNEE = rmse(dat30day_NEE-mod30day_NEE)
RMSE_30dayNDVI = rmse(dat30day_NDVI-mod30day_NDVI)
R2_30dayNEE = cor(dat30day_NEE,mod30day_NEE)^2
R2_30dayNDVI = cor(dat30day_NDVI,mod30day_NDVI)^2


#calculate 90 day modelled and measured
nr = length(out.compare_NEE$NEE)
gr = rep(1:floor(nr/90), each = 90)
mod90day_NEE = aggregate(out.compare_NEE$NEE[-c(1:37)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NEE$NEE)
gr = rep(1:floor(nr/90), each = 90)
dat90day_NEE = aggregate(data.compare_NEE$NEE[-c(1:37)] ~ gr, FUN=mean)[,-1]

nr = length(out.compare_NDVI$NDVI)
gr = rep(1:floor(nr/90), each = 90)
mod90day_NDVI = aggregate(out.compare_NDVI$NDVI[-c(1:39)] ~ gr, FUN=mean)[,-1]
nr = length(data.compare_NDVI$NDVI)
gr = rep(1:floor(nr/90), each = 90)
dat90day_NDVI = aggregate(data.compare_NDVI$NDVI[-c(1:39)] ~ gr, FUN=mean)[,-1]


RMSE_90dayNEE = rmse(dat90day_NEE-mod90day_NEE)
RMSE_90dayNDVI = rmse(dat90day_NDVI-mod90day_NDVI)
R2_90dayNEE = cor(dat90day_NEE,mod90day_NEE)^2
R2_90dayNDVI = cor(dat90day_NDVI,mod90day_NDVI)^2


#side by side barplot
names=c("1", "8", "14", "30", "90")
bars <- matrix(1, 4, length(names))
bars[1,]=c(RMSE_NEE, RMSE_8dayNEE, RMSE_14dayNEE, RMSE_30dayNEE, RMSE_90dayNEE)
bars[2,]=c(RMSE_NDVI*10, RMSE_8dayNDVI*10, RMSE_14dayNDVI*10, RMSE_30dayNDVI*10, RMSE_90dayNDVI*10)
bars[3,]=c(R2_NEE, R2_8dayNEE, R2_14dayNEE, R2_30dayNEE, R2_90dayNEE)
bars[4,]=c(R2_NDVI, R2_8dayNDVI, R2_14dayNDVI, R2_30dayNDVI, R2_90dayNDVI)
colnames(bars)=names
head(bars)

par(mfrow=c(1,1), mar=c(8,15,3,6),las=0)
barplot(bars[1:2,], cex.lab=2.5, axes=FALSE, cex.names=2, ylab = "", xlab="", col=c("gray50","gray20"),beside=TRUE, ylim=c(0, 1))
title(xlab="Time Averaged (days)", line=3.5, cex.lab=2.5)
axis(2, lwd=2, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1), line=1, col="gray50", cex.axis=2.5, las=1, col.axis="gray50")
axis(2, lwd=2, at=c(0,0.2,0.4,0.6), labels=c(0,0.02,0.04,0.06), line=5.5, col="gray20", cex.axis=2.5, las=1, col.axis="gray20")
title(ylab="NDVI RMSE                            ", line=11, cex.lab=2.5, col.lab="gray20")
title(ylab="                                       NEE RMSE", line=5, cex.lab=2.5, col.lab="gray50")
par(new=TRUE, mar=c(6.7,11,1.9,6))
plot(bars[3,]~c(1,1.9,2.8,3.7,4.6), pch=1, cex=1.5, xlim=c(0,5), ylim=c(0,1),axes=FALSE, xlab="", ylab="")
lines(bars[3,]~c(1,1.9,2.8,3.7,4.6), xlim=c(0,5), lwd=2)
par(new=TRUE)
plot(bars[4,]~c(1.3,2.2,3.1,4,4.9), cex=1.5, pch=19, xlim=c(0,5), ylim=c(0,1),axes=FALSE, xlab="", ylab="")
lines(bars[4,]~c(1.3,2.2,3.1,4,4.9), xlim=c(0,5), lwd=2)
axis(4, lwd=2, at=c(0,0.2,0.4,0.6,0.8,1), labels=c(0,0.2,0.4,0.6,0.8,1), line=1, col="black", cex.axis=2.5, las=1, col.axis="black")


################Spatial Validation######################

dat = data.frame(read.csv("Summary_AllSites.csv")) #select data file
head(dat) #fiew first 6 rows


#put it all into a table
summary = data.frame(matrix(1, 1, 16))
colnames(summary) = c("Latitude", "Biomass_C", "Biomass_N", "SOM_C", "SOM_N", "Available_N", "CCaN_NDVImax", "CCaN_LAImax", "MODIS_max", "CCaN_NDVIavg", "CCaN_LAIavg", "MODIS_avg", "MODIS_sd", "Tmax", "Tavg", "PARavg")
head(summary)

#########
latitudes = unique(dat$Latitude)
for(i in 1:length(latitudes)){
  lat.i = latitudes[i]  
  dat.i = subset(dat, Latitude==lat.i)
  
  time=seq(1:length(dat.i[,1])) #generate a sequence of x values for interpolation
  LST.filled = approx(time, dat.i$LST_avg, time, method = "linear", rule = 2)$y #fill LST
  LST.filled = LST.filled-273.15 #convert to celcius
  
  PAR.filled = approx(time, dat.i$PAR, time, method = "linear", rule = 2)$y #fill PAR
  PAR.filled = 3.5947*PAR.filled #convert to mol m-2 s-1
  
  NDVI.filled = approx(time, dat.i$NDVI, time, method = "linear", rule = 2)$y #fill NDVI
  
  #bind these columns to dat
  dat.i=cbind(dat.i, LST.filled, PAR.filled, NDVI.filled)
  
  #Step 2: calculate decadal averages
  LST.avg = tapply(dat.i$LST.filled, dat.i$DOY, mean)
  PAR.avg = tapply(dat.i$PAR.filled, dat.i$DOY, mean)
  NDVI.avg = tapply(dat.i$NDVI.filled, dat.i$DOY, mean)
  DOY=seq(1:366)
  Year=rep(2000,length(DOY))
  data=data.frame(Year,DOY,LST.avg,PAR.avg,NDVI.avg)
  
  #Step 3: get ready for model input
  #calculate scalars
  
  #seasonality scalar
  #DOY of senescence 
  sen.day=min(data$DOY[which(data$LST.avg<=10 & data$DOY>200)])
  sen.day
  num.days = 366
  senDOY = rep(sen.day, num.days)
  data = data.frame(data, senDOY = senDOY)
  
  #start day
  start.day=min(data$DOY[which(data$LST.avg>=-5 & data$DOY>120)])
  start.day
  startDOY = rep(start.day, num.days)
  data = data.frame(data, startDOY = startDOY)
  
  #end day
  end.day=min(data$DOY[which(data$LST.avg<=0 & data$DOY>240)])
  end.day
  endDOY = rep(end.day, num.days)
  data = data.frame(data, endDOY = endDOY)
  
  
  
  #create scalar
  scal.seas=rep(1, length(data$DOY))
  for (i in 1:length(data$DOY)){
    if(data$DOY[i]<data$startDOY[i]){ #prior to snow melt
      scal.seas[i]=0
    }
    if(data$DOY[i]>=data$startDOY[i]){ #after melt
      if(data$DOY[i]<=data$senDOY[i]){ #prior to peak
        slope = 1/(data$senDOY[i]-data$startDOY[i])
        scal.seas[i] = 0+(slope*(data$DOY[i]-data$startDOY[i]))
      }
      if(data$DOY[i]>data$senDOY[i] & data$DOY[i]<data$endDOY[i]){ #after peak but before frost
        slope = 1/(data$endDOY[i]-data$senDOY[i])
        scal.seas[i] = 0+(slope*(data$endDOY[i]-data$DOY[i]))
      }
      if(data$DOY[i]>=data$endDOY[i]){ #after frost
        scal.seas[i]=0
      }
    }
  }
  
  
  #temperature scalar
  Tmax = max(data$LST.avg)
  Tmin = min(data$LST.avg)
  scal.temp=NULL
  for (i in 1:length(data$LST.avg)){
    scal.temp[i] = (data$LST.avg[i] - Tmin)/(Tmax-Tmin) 
  }
  
  
  #calculate growing season average temp
  Temp_GS = data$LST.avg[data$DOY>=data$startDOY[1] & data$DOY <= data$endDOY[1]]
  Temp_avg = mean(Temp_GS)
  
  #Run spinup
  numyears = 30
  DOY.spin = rep(data$DOY, numyears)
  Year.spin = rep(data$Year, numyears)
  LST.spin = rep(data$LST.avg, numyears)
  PAR.spin = rep(data$PAR.avg, numyears)
  scal.temp.spin = rep(scal.temp, numyears)
  scal.seas.spin = rep(scal.seas, numyears)
  TempAvg.spin = rep(Temp_avg, length(DOY.spin))
  
  
  time = seq(1:length(DOY.spin))
  
  #Step 4: make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=LST.spin, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
  scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=scal.seas.spin, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
  
  
  #OPEN 3_Model.R and run it the first time
  
  params = param.best
  
  state = state.best
  
  out.spin= data.frame(solvemodel(params, state)) #creates table of model output
  
  #Run model for each site and record NDVI
  
  time = seq(1:length(data$DOY))
  
  #Step 4: make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=data$LST.avg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=data$PAR.avg, method="linear", rule=2)
  scaltemp.d1 <- approxfun(x=time, y=scal.temp, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=data$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=data$Year, method="linear", rule=2)
  
  
  #adjust starting values
  end.time = length(out.spin[,1])
  #adjust starting values
  state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
              Biomass_N = out.spin$Biomass_N[end.time], 
              SOM_C = out.spin$SOM_C[end.time], 
              SOM_N = out.spin$SOM_N[end.time],
              Available_N = out.spin$Available_N[end.time])
  
  
  out= data.frame(solvemodel(params, state)) #creates table of model output
  
  #determine max NDVI & record in spreadsheet
  CCaN_NDVImax = max(out$NDVI)
  CCaN_LAImax = max(out$LAI)
  MODIS_max = max(data$NDVI)
  
  #determine GS average NDVI 
  #pull out GS data (DOY > 160 & DOY < 245)
  CCaN_gsNDVI = out$NDVI[out$DOY>=150 & out$DOY <=240]
  CCaN_gsLAI = out$LAI[out$DOY>=150 & out$DOY <=240]
  MODIS_gsNDVI = data$NDVI.avg[data$DOY>=150 & data$DOY <=240]
  CCaN_NDVIavg = mean(CCaN_gsNDVI)
  CCaN_LAIavg = mean(CCaN_gsLAI)
  MODIS_avg = mean(MODIS_gsNDVI)
  MODIS_sd = sd(MODIS_gsNDVI)
  
  #determine GS Avg Temp & PAR 
  #pull out GS data (DOY > 140 & DOY < 250)
  Temp_GS = data$LST.avg[data$DOY>=150 & data$DOY <=240]
  PAR_GS = data$PAR.avg[data$DOY>=150 & data$DOY <=240]
  #calculate average
  Tavg = mean(Temp_GS)
  PARavg = mean(PAR_GS)
  
  summary = rbind(summary, c(lat.i, state, CCaN_NDVImax, CCaN_LAImax, MODIS_max, CCaN_NDVIavg, CCaN_LAIavg, MODIS_avg, MODIS_sd, Tmax, Tavg, PARavg)) #bind new row to table
}

#############
summary=summary[-1,]
length(summary[,1])
write.csv(summary, "CaTT_Summary_081216") #save CSV 

#plots of spatial data
par(mfrow=c(2,1), mar=c(4,5,2,2))
plot(summary$Tavg~summary$Latitude, pch=16, xlab="Latitude", ylab="GS Avg Temp")
plot(summary$PARavg~summary$Latitude, pch=16, xlab="Latitude", ylab="GS Avg PAR")

#modelled vs. measured
reg_NDVI = lm(summary$MODIS_avg~summary$CCaN_avg)
summary(reg_NDVI)

#modelled vs temp
reg_modtemp = lm(summary$CCaN_avg~summary$Tavg)
summary(reg_modtemp)

#measured vs temp
reg_meastemp = lm(summary$MODIS_avg~summary$Tavg)
summary(reg_meastemp)


head(summary)




summary=read.csv("CaTT_Summary_080416")
NDVI.CCaNuncertainty = read.csv("CaTT_uncertainty_summary")
head(NDVI.CCaNuncertainty)
head(summary)

par(mar=c(6,6,2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = FALSE), widths=c(3,1.5))

plot(summary$CCaN_avg~summary$Latitude, pch=16, cex=2, cex.lab=3, xlab="", ylab="", ylim=c(0.2,0.8), xlim=c(65,71), axes=FALSE)
with(NDVI.CCaNuncertainty,polygon(c(Latitude, rev(Latitude)),c(p05,rev(p95)),col = adjustcolor("gray75",alpha.f=0.75), border = adjustcolor("gray75",alpha.f=0.75)))
points(summary$CCaN_avg~summary$Latitude, pch=16, col="gray50", cex=2)
#with(summary,polygon(c(Latitude, rev(Latitude)),c(MODIS_avg-(MODIS_sd/sqrt(32)*1.65),rev(MODIS_avg+(MODIS_sd/sqrt(32)*1.65))),col = adjustcolor("gray30",alpha.f=0.75), border = adjustcolor("gray75",alpha.f=0.75)))
with(summary, arrows(Latitude,MODIS_avg+(MODIS_sd/sqrt(32)*1.65), Latitude, MODIS_avg-(MODIS_sd/sqrt(32)*1.65), angle=90, code=3, length=0.05, col="black", lwd=2))
points(summary$MODIS_avg~summary$Latitude, pch=17, col="black", cex=2)
axis(1, at=c(65,67,69,71), labels = c(65,67,69,71), cex.axis = 3, line=0, padj=0.5, lwd=2)
axis(2, at=c(0.2,0.4, 0.6, 0.8), labels=c(0.2,0.4,0.6,0.8), cex.axis = 3, line=0, lwd=2)
legend(65,0.4, cex=3, legend=c("CCaN", "MODIS"), bty="n", pch=c(16,17), col=c("gray50","black"))

plot(summary$Tavg,summary$MODIS_avg, pch=17, col="black", cex=2, cex.lab=2, ylim=c(0.3,0.8), xlim=c(8,18),xlab="", ylab="",axes=FALSE)
points(summary$Tavg,summary$CCaN_avg, pch=16, col="gray50", cex=2)
axis(1, at=c(8,18), labels=c(8,18), cex.axis=2, padj=0.5, lwd=2)
axis(2, at=c(0.3,0.8), labels=c(0.3,0.8), cex.axis=2, lwd=2)
abline(reg_modtemp, lwd=4, col="gray50")
abline(reg_meastemp, lwd=4, col="black")

plot(summary$CCaN_avg,summary$MODIS_avg, pch=16, cex=2, cex.lab=1.5, xlab="", ylab="", ylim=c(0.3,0.8), xlim=c(0.3,0.8), axes=FALSE)
axis(1, at=c(0.3,0.8), labels=c(0.3,0.8), cex.axis=2, lwd=2, padj=0.5)
axis(2, at=c(0.3,0.8), labels=c(0.3,0.8), cex.axis=2, lwd=2)
abline(0,1, col="gray50", lty=2, lwd=4)
abline(reg_NDVI, lwd=4)



##########################VARIANCE DECOMPOSITION ANALYSIS#######################

head(param.keep) #view table of accepted parameters
summarytable
means



#to perform the variance decomposition analysis, you need to:
# 1) alter each parameter individually holding all other parameters constant at their means
# 2) run the model for each parameter set to obtain an ensemble of model runs
# 3) for each model run, calculate the monthly average of the output
# 4) Calculate the variance in monthly averages for each parameter - this gives you the contribution of that parameter to the model variance

state=state.best
out = data.frame(solvemodel(param.best, state))
head(out[,1:11])


####SEASONAL ANALYSIS####
#need to make tables to store monthly averages for each model output (each parameter has one table)

MVar_kplant = data.frame(matrix(1,1,11))
colnames(MVar_kplant)=c("Month", colnames(out[,2:11]))
MVar_LitterRate = data.frame(matrix(1,1,11))
colnames(MVar_LitterRate)=c("Month", colnames(out[,2:11]))
MVar_UptakeRate = data.frame(matrix(1,1,11))
colnames(MVar_UptakeRate)=c("Month", colnames(out[,2:11]))
MVar_propN_fol = data.frame(matrix(1,1,11))
colnames(MVar_propN_fol)=c("Month", colnames(out[,2:11]))
MVar_propN_roots = data.frame(matrix(1,1,11))
colnames(MVar_propN_roots)=c("Month", colnames(out[,2:11]))
MVar_netNrate = data.frame(matrix(1,1,11))
colnames(MVar_netNrate)=c("Month", colnames(out[,2:11]))
MVar_q10 = data.frame(matrix(1,1,11))
colnames(MVar_q10)=c("Month", colnames(out[,2:11]))

#need to create a vector of months to append to model output
months = rep(c(seq(1:12)),
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
months.leap = rep(c(seq(1:12)),
                  c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

months = c(months.leap, months, months, months, months.leap, months, months, months)

time = seq(1:length(data$time))

#kplant
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_kplant)
  MVar_kplant = rbind(MVar_kplant, monthly.avg)
}  

#LitterRate
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_LitterRate)
  MVar_LitterRate = rbind(MVar_LitterRate, monthly.avg)
}  

#UptakeRate
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_UptakeRate)
  MVar_UptakeRate = rbind(MVar_UptakeRate, monthly.avg)
}  

#propN_fol
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_fol)
  MVar_propN_fol = rbind(MVar_propN_fol, monthly.avg)
}  

#propN_roots
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_roots)
  MVar_propN_roots = rbind(MVar_propN_roots, monthly.avg)
}  

#netNrate
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_netNrate)
  MVar_netNrate = rbind(MVar_netNrate, monthly.avg)
}  

#q10
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_q10)
  MVar_q10 = rbind(MVar_q10, monthly.avg)
}  


MVar_kplant = MVar_kplant[-1,]
MVar_LitterRate = MVar_LitterRate[-1,]
MVar_UptakeRate = MVar_UptakeRate[-1,]
MVar_propN_fol = MVar_propN_fol[-1,]
MVar_propN_roots = MVar_propN_roots[-1,]
MVar_netNrate = MVar_netNrate[-1,]
MVar_q10 = MVar_q10[-1,]

var.kplant = aggregate(MVar_kplant[,2:11], list(MVar_kplant$Month), var)
var.LitterRate = aggregate(MVar_LitterRate[,2:11], list(MVar_LitterRate$Month), var)
var.UptakeRate = aggregate(MVar_UptakeRate[,2:11], list(MVar_UptakeRate$Month), var)
var.propN_fol = aggregate(MVar_propN_fol[,2:11], list(MVar_propN_fol$Month), var)
var.propN_roots = aggregate(MVar_propN_roots[,2:11], list(MVar_propN_roots$Month), var)
var.netNrate = aggregate(MVar_netNrate[,2:11], list(MVar_netNrate$Month), var)
var.q10 = aggregate(MVar_q10[,2:11], list(MVar_q10$Month), var)

parameters = rep(names(params), c(12,12,12,12,12,12,12))

all = rbind(var.kplant, var.LitterRate, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.netNrate, var.q10)

all=cbind(Parameters = parameters, all)
head(all)

#calculate total variance
var.total = aggregate(all[3:12], list(all$Group.1), sum)  #CHECK THIS

#now calculate percent variance
perc.kplant = (var.kplant[,2:11]/var.total[,2:11])*100
perc.kplant = cbind(Parameter = rep("kplant", 12), Month=var.total$Group.1, perc.kplant)

perc.LitterRate = (var.LitterRate[,2:11]/var.total[,2:11])*100
perc.LitterRate = cbind(Parameter = rep("LitterRate", 12), Month=var.total$Group.1, perc.LitterRate)

perc.UptakeRate = (var.UptakeRate[,2:11]/var.total[,2:11])*100
perc.UptakeRate = cbind(Parameter = rep("UptakeRate", 12), Month=var.total$Group.1, perc.UptakeRate)

perc.propN_fol = (var.propN_fol[,2:11]/var.total[,2:11])*100
perc.propN_fol = cbind(Parameter = rep("propN_fol", 12), Month=var.total$Group.1, perc.propN_fol)

perc.propN_roots = (var.propN_roots[,2:11]/var.total[,2:11])*100
perc.propN_roots = cbind(Parameter = rep("propN_roots", 12), Month=var.total$Group.1, perc.propN_roots)

perc.netNrate = (var.netNrate[,2:11]/var.total[,2:11])*100
perc.netNrate = cbind(Parameter = rep("netNrate", 12), Month=var.total$Group.1, perc.netNrate)

perc.q10 = (var.q10[,2:11]/var.total[,2:11])*100
perc.q10 = cbind(Parameter = rep("q10", 12), Month=var.total$Group.1, perc.q10)

#create a table binding all together

perc.all = rbind(perc.kplant, perc.LitterRate, perc.UptakeRate, 
                 perc.propN_fol, perc.propN_roots, perc.netNrate, perc.q10)

head(perc.all)
perc.all = perc.all[,-11]
head(perc.all)
tail(perc.all)

####barplots####

par(mfrow=c(4,2), mar=c(4,3,2,1))
names = c("kplant", "LitterRate", "UptakeRate", "propN_fol0", "propN_roots", "netNrate", "Q10")
main.names = c("X", "X", "Biomass C", "Biomass N", "SOM C", "SOM N", "Available N", "NEE", "X", "X", "NDVI")

for (n in c(3:8,11)) { #for each output
  sub = perc.all[,c(1,2,n)]
  sub1 = table(sub$Parameter, sub$Month)
  sub1[1,] = sub[1:12,3]
  sub1[2,] = sub[13:24,3]
  sub1[3,] = sub[25:36,3]
  sub1[4,] = sub[37:48,3]
  sub1[5,] = sub[49:60,3]
  sub1[6,] = sub[61:72,3]
  sub1[7,] = sub[73:84,3]
  barplot(sub1, col=c("darkolivegreen3", "aquamarine", "maroon4", "mediumseagreen",
                      "darkslategray", "darkblue", "gray50"),             
          main=main.names[n], cex.main=1.5, names.arg=c("J","F","M","A","M","J","J","A","S","O","N","D"), axisnames=TRUE, ylim=c(0,100), cex.names=1.25, cex.axis=1.25) #plot the data
} #end of for loop
plot(NULL, axes=FALSE)
legend("topleft", legend=names, pch=15, col=c("darkolivegreen3", "aquamarine", "maroon4", "mediumseagreen",
                                   "darkslategray", "darkblue", "gray50"), cex=1.5, bty="n", ncol=2)

#NDVI
par(mfrow=c(1,1))
sub = perc.all[,c(1,2,11)]
sub1 = table(sub$Parameter, sub$Month)
sub1[1,] = sub[1:12,3]
sub1[2,] = sub[13:24,3]
sub1[3,] = sub[25:36,3]
sub1[4,] = sub[37:48,3]
sub1[5,] = sub[49:60,3]
sub1[6,] = sub[61:72,3]
barplot(sub1, col=c("darkolivegreen3", "aquamarine", "maroon4", "mediumseagreen",
                    "palegreen", "darkblue", "gray"),           
        main=names(perc.all[11]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100), legend=TRUE, args.legend=c(horiz=FALSE)) #plot the data


####ANNUAL ANALYSIS####
#need to make tables to store annual sums for each model output (each parameter has one table)

AVar_kplant = data.frame(matrix(1,1,3))
colnames(AVar_kplant)=colnames(out[,7:9])
AVar_LitterRate = data.frame(matrix(1,1,3))
colnames(AVar_LitterRate)=colnames(out[,7:9])
AVar_UptakeRate = data.frame(matrix(1,1,3))
colnames(AVar_UptakeRate)=colnames(out[,7:9])
AVar_propN_fol = data.frame(matrix(1,1,3))
colnames(AVar_propN_fol)=colnames(out[,7:9])
AVar_propN_roots = data.frame(matrix(1,1,3))
colnames(AVar_propN_roots)=colnames(out[,7:9])
AVar_netNrate = data.frame(matrix(1,1,3))
colnames(AVar_netNrate)=colnames(out[,7:9])
AVar_q10 = data.frame(matrix(1,1,3))
colnames(AVar_q10)=colnames(out[,7:9])

AVar_kplant_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_kplant_NDVI)=c("NDVI")
AVar_LitterRate_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_LitterRate_NDVI)=c("NDVI")
AVar_UptakeRate_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_UptakeRate_NDVI)=c("NDVI")
AVar_propN_fol_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_propN_fol_NDVI)=c("NDVI")
AVar_propN_roots_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_propN_roots_NDVI)=c("NDVI")
AVar_netNrate_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_netNrate_NDVI)=c("NDVI")
AVar_q10_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_q10_NDVI)=c("NDVI")

head(summarytable)
q15 = apply(param.keep, 2, quantile, 0.15)
q85 = apply(param.keep, 2, quantile, 0.85)
q40 = apply(param.keep, 2, quantile, 0.40)
q60 = apply(param.keep, 2, quantile, 0.60)
summarytable = data.frame(q05=summarytable[,1], q95=summarytable[,5], q15, q85, q25=summarytable[,2], q75=summarytable[,4], q40, q60)
head(summarytable)

param.keep_NEE_NDVI_UNBdata = param.keep

first=1
second=2

#kplant
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[1,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[1,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate avg sum across all years
  names(annual.avg) = names(AVar_kplant) #change names
  AVar_kplant = rbind(AVar_kplant, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_kplant_NDVI) #change names
  AVar_kplant_NDVI = rbind(AVar_kplant_NDVI, annual.avgNDVI) #add row to table
}  

#LitterRate
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$LitterRate>=summarytable[2,first] & param.keep_NEE_NDVI_UNBdata$LitterRate<=summarytable[2,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_LitterRate) #change names
  AVar_LitterRate = rbind(AVar_LitterRate, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_LitterRate_NDVI) #change names
  AVar_LitterRate_NDVI = rbind(AVar_LitterRate_NDVI, annual.avgNDVI) #add row to table
}  

#UptakeRate
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$UptakeRate>=summarytable[3,first] & param.keep_NEE_NDVI_UNBdata$UptakeRate<=summarytable[3,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_UptakeRate) #change names
  AVar_UptakeRate = rbind(AVar_UptakeRate, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_UptakeRate_NDVI) #change names
  AVar_UptakeRate_NDVI = rbind(AVar_UptakeRate_NDVI, annual.avgNDVI) #add row to table
}  


#propN_fol
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$propN_fol>=summarytable[4,first] & param.keep_NEE_NDVI_UNBdata$propN_fol<=summarytable[4,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_propN_fol) #change names
  AVar_propN_fol = rbind(AVar_propN_fol, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_propN_fol_NDVI) #change names
  AVar_propN_fol_NDVI = rbind(AVar_propN_fol_NDVI, annual.avgNDVI) #add row to table
}  

#propN_roots
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$propN_roots>=summarytable[5,first] & param.keep_NEE_NDVI_UNBdata$propN_roots<=summarytable[5,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_propN_roots) #change names
  AVar_propN_roots = rbind(AVar_propN_roots, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_propN_roots_NDVI) #change names
  AVar_propN_roots_NDVI = rbind(AVar_propN_roots_NDVI, annual.avgNDVI) #add row to table
}  

#netNrate
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$netNrate>=summarytable[6,first] & param.keep_NEE_NDVI_UNBdata$netNrate<=summarytable[6,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_netNrate) #change names
  AVar_netNrate = rbind(AVar_netNrate, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_netNrate_NDVI) #change names
  AVar_netNrate_NDVI = rbind(AVar_netNrate_NDVI, annual.avgNDVI) #add row to table
}  

#q10
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$q10>=summarytable[6,first] & param.keep_NEE_NDVI_UNBdata$q10<=summarytable[6,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_q10) #change names
  AVar_q10 = rbind(AVar_q10, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_q10_NDVI) #change names
  AVar_q10_NDVI = rbind(AVar_q10_NDVI, annual.avgNDVI) #add row to table
}  


AVar_kplant = AVar_kplant[-1,]
AVar_LitterRate = AVar_LitterRate[-1,]
AVar_UptakeRate = AVar_UptakeRate[-1,]
AVar_propN_fol = AVar_propN_fol[-1,]
AVar_propN_roots = AVar_propN_roots[-1,]
AVar_netNrate = AVar_netNrate[-1,]
AVar_q10 = AVar_q10[-1,]

var.kplant = apply(AVar_kplant, 2, var)
var.LitterRate = apply(AVar_LitterRate, 2, var)
var.UptakeRate = apply(AVar_UptakeRate, 2, var)
var.propN_fol = apply(AVar_propN_fol, 2, var)
var.propN_roots = apply(AVar_propN_roots, 2, var)
var.netNrate = apply(AVar_netNrate, 2, var)
var.q10 = apply(AVar_q10, 2, var)

AVar_kplant_NDVI = AVar_kplant_NDVI[-1,]
AVar_LitterRate_NDVI  = AVar_LitterRate_NDVI[-1,]
AVar_UptakeRate_NDVI  = AVar_UptakeRate_NDVI[-1,]
AVar_propN_fol_NDVI  = AVar_propN_fol_NDVI[-1,]
AVar_propN_roots_NDVI  = AVar_propN_roots_NDVI[-1,]
AVar_netNrate_NDVI  = AVar_netNrate_NDVI[-1,]
AVar_q10_NDVI  = AVar_q10_NDVI[-1,]

var.kplant_NDVI = var(AVar_kplant_NDVI)
var.LitterRate_NDVI = var(AVar_LitterRate_NDVI)
var.UptakeRate_NDVI = var(AVar_UptakeRate_NDVI)
var.propN_fol_NDVI = var(AVar_propN_fol_NDVI)
var.propN_roots_NDVI = var(AVar_propN_roots_NDVI)
var.netNrate_NDVI = var(AVar_netNrate_NDVI)
var.q10_NDVI = var(AVar_q10_NDVI)


all_NEE = rbind(var.kplant, var.LitterRate, var.UptakeRate, var.propN_fol, 
                var.propN_roots, var.netNrate, var.q10)
all_NDVI = rbind(var.kplant_NDVI, var.LitterRate_NDVI, var.UptakeRate_NDVI, var.propN_fol_NDVI, 
                 var.propN_roots_NDVI, var.netNrate_NDVI, var.q10_NDVI)

rownames(all_NEE)=names(params)
rownames(all_NDVI)=names(params)

#calculate total variance
var.total = apply(all_NEE, 2, sum)
var.total_NDVI = apply(all_NDVI, 2, sum)

#now calculate percent variance
perc.kplant = (var.kplant/var.total)*100
perc.LitterRate = (var.LitterRate/var.total)*100
perc.UptakeRate = (var.UptakeRate/var.total)*100
perc.propN_fol = (var.propN_fol/var.total)*100
perc.propN_roots = (var.propN_roots/var.total)*100
perc.netNrate = (var.netNrate/var.total)*100
perc.q10 = (var.q10/var.total)*100

perc.kplant_NDVI = (var.kplant_NDVI/var.total_NDVI)*100
perc.LitterRate_NDVI = (var.LitterRate_NDVI/var.total_NDVI)*100
perc.UptakeRate_NDVI = (var.UptakeRate_NDVI/var.total_NDVI)*100
perc.propN_fol_NDVI = (var.propN_fol_NDVI/var.total_NDVI)*100
perc.propN_roots_NDVI = (var.propN_roots_NDVI/var.total_NDVI)*100
perc.netNrate_NDVI = (var.netNrate_NDVI/var.total_NDVI)*100
perc.q10_NDVI = (var.q10_NDVI/var.total_NDVI)*100


#create a table binding all together

perc.all_NEE = rbind(perc.kplant, perc.LitterRate, perc.UptakeRate, 
                     perc.propN_fol, perc.propN_roots, perc.netNrate, perc.q10)

rownames(perc.all_NEE)=c(names(params))

perc.all_NDVI = rbind(perc.kplant_NDVI, perc.LitterRate_NDVI, perc.UptakeRate_NDVI, 
                      perc.propN_fol_NDVI, perc.propN_roots_NDVI, perc.netNrate_NDVI, perc.q10_NDVI)

rownames(perc.all_NDVI)=c(names(params))
colnames(perc.all_NDVI)=c("NDVI")



#store for this subset
perc.all.NEE_100 = data.frame(perc.all_NEE)
perc.all.NDVI_100 = data.frame(perc.all_NDVI)

####barplots####
#side by side barplot

sensvars = c("NEE",
             "NDVI")

#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=param.best, state=state.best, sensvar = sensvars)
head(s.local); tail(s.local)
s.local.summ = data.frame(summary(s.local, var=T))
head(s.local.summ); tail(s.local.summ)
s.loc.summ.ordered = data.frame(s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] )
#make a bar graph 

#plot
names = c("kplant", "LitterRate", "UptakeRate", "propN_fol0", "propN_roots", "netNrate", "Q10")
bars <- matrix(1, 2, length(names))
bars[1,]=abs(subset(s.local.summ, var=="NEE")$L1)
bars[2,]=abs(subset(s.local.summ, var=="NDVI")$L1)
colnames(bars)=names
head(bars)
par(las=1)
par(mfrow=c(2,1), mar=c(4,11,1,2))
barplot(bars, cex.lab=2, cex.axis=2, cex.names=2, horiz=TRUE, col=c("gray50","gray20"),xlab="Model Sensitivity", beside=TRUE, xlim=c(0, 1.2))


bars <- matrix(1, 2, length(names))
bars[2,]=perc.all.NEE_100$NEE
bars[1,]=perc.all.NDVI_100$NDVI
colnames(bars)=names
head(bars)
barplot(bars, cex.lab=2, cex.axis=2, horiz=TRUE, cex.names=2, xlab = "% of Total Annual Variance", col=c("gray50","gray20"),beside=TRUE,xlim=c(0, 50))

#correlation plots

head(param.keep)
correlations = c(cor(param.keep))
correlations
correlations=round(correlations, digits=3)
correlations[abs(correlations)<0.05]<-"<0.05"
correlations
matrix(correlations, 7,7)

par(mfrow=c(7,7), oma=c(5,5,3,3), mar=c(0.5,0.5,0.5,0.5))
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[1], cex=1.5, font=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[2], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[3], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[4], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[5], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[6], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[7], cex=1.5)
plot(param.keep[,1], param.keep[,2], xlim=c(0.25*min(param.keep[,1]), 1.25*max(param.keep[,1])),
     ylim=c(0.25*min(param.keep[,2]), 1.25*max(param.keep[,2])), xaxt="n", yaxt="n")
  axis(2,las=1, at=c(0, max(param.keep[,2])),
     labels=c(0, round(max(param.keep[,2]), 3)), cex.axis=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[2], cex=1.5, font=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[10], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[11], cex=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[12], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[13], cex=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[14], cex=1.5)
plot(param.keep[,1], param.keep[,3], xlim=c(0.25*min(param.keep[,1]), 1.25*max(param.keep[,1])),
     ylim=c(0.25*min(param.keep[,3]), 1.25*max(param.keep[,3])), xaxt="n", yaxt="n")
axis(2,las=1, at=c(0, max(param.keep[,3])),
     labels=c(0, round(max(param.keep[,3]), 3)), cex.axis=1.5)
plot(param.keep[,2], param.keep[,3], xlim=c(0.25*min(param.keep[,2]), 1.25*max(param.keep[,2])),
     ylim=c(0.25*min(param.keep[,3]), 1.25*max(param.keep[,3])), xaxt="n", yaxt="n")
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[3], cex=1.5, font=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[18], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[19], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[20], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[21], cex=1.5)
plot(param.keep[,1], param.keep[,4], xlim=c(0.25*min(param.keep[,1]), 1.25*max(param.keep[,1])),
     ylim=c(0.25*min(param.keep[,4]), 1.25*max(param.keep[,4])), xaxt="n", yaxt="n")
axis(2,las=1, at=c(0, max(param.keep[,4])),
     labels=c(0, round(max(param.keep[,4]), 3)), cex.axis=1.5)
plot(param.keep[,2], param.keep[,4], xlim=c(0.25*min(param.keep[,2]), 1.25*max(param.keep[,2])),
     ylim=c(0.25*min(param.keep[,4]), 1.25*max(param.keep[,4])), xaxt="n", yaxt="n")
plot(param.keep[,3], param.keep[,4], xlim=c(0.25*min(param.keep[,3]), 1.25*max(param.keep[,3])),
     ylim=c(0.25*min(param.keep[,4]), 1.25*max(param.keep[,4])), xaxt="n", yaxt="n")
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[4], cex=1.5, font=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[26], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[27], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[28], cex=1.5)
plot(param.keep[,1], param.keep[,5], xlim=c(0.25*min(param.keep[,1]), 1.25*max(param.keep[,1])),
     ylim=c(0.25*min(param.keep[,5]), 1.25*max(param.keep[,5])), xaxt="n", yaxt="n")
axis(2,las=1, at=c(min(param.keep[,5]), max(param.keep[,5])),
     labels=c(round(min(param.keep[,5]), 3), round(max(param.keep[,5]), 3)), cex.axis=1.5)
plot(param.keep[,2], param.keep[,5], xlim=c(0.25*min(param.keep[,2]), 1.25*max(param.keep[,2])),
     ylim=c(0.25*min(param.keep[,5]), 1.25*max(param.keep[,5])), xaxt="n", yaxt="n")
plot(param.keep[,3], param.keep[,5], xlim=c(0.25*min(param.keep[,3]), 1.25*max(param.keep[,3])),
     ylim=c(0.25*min(param.keep[,5]), 1.25*max(param.keep[,5])), xaxt="n", yaxt="n")
plot(param.keep[,4], param.keep[,5], xlim=c(0.25*min(param.keep[,4]), 1.25*max(param.keep[,4])),
     ylim=c(0.25*min(param.keep[,5]), 1.25*max(param.keep[,5])), xaxt="n", yaxt="n")
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[5], cex=1.5, font=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[34], cex=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[35], cex=1.5)
plot(param.keep[,1], param.keep[,6], xlim=c(0.25*min(param.keep[,1]), 1.25*max(param.keep[,1])),
     ylim=c(0.25*min(param.keep[,6]), 1.25*max(param.keep[,6])), xaxt="n", yaxt="n")
axis(2,las=1, at=c(5E-7, 2E-5),
     labels=c("5E-7", "2E-5"), cex.axis=1.5)
plot(param.keep[,2], param.keep[,6], xlim=c(0.25*min(param.keep[,2]), 1.25*max(param.keep[,2])),
 ylim=c(0.25*min(param.keep[,6]), 1.25*max(param.keep[,6])), xaxt="n", yaxt="n")
plot(param.keep[,3], param.keep[,6], xlim=c(0.25*min(param.keep[,3]), 1.25*max(param.keep[,3])),
     ylim=c(0.25*min(param.keep[,6]), 1.25*max(param.keep[,6])), xaxt="n", yaxt="n")
plot(param.keep[,4], param.keep[,6], xlim=c(0.25*min(param.keep[,4]), 1.25*max(param.keep[,4])),
     ylim=c(0.25*min(param.keep[,6]), 1.25*max(param.keep[,6])), xaxt="n", yaxt="n")
plot(param.keep[,5], param.keep[,6], xlim=c(0.25*min(param.keep[,5]), 1.25*max(param.keep[,5])),
     ylim=c(0.25*min(param.keep[,6]), 1.25*max(param.keep[,6])), xaxt="n", yaxt="n")
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[6], cex=1.5, font=2)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n", yaxt="n")
text(0,0,correlations[42], cex=1.5)
plot(param.keep[,1], param.keep[,7], xlim=c(0.25*min(param.keep[,1]), 1.25*max(param.keep[,1])),
     ylim=c(0.25*min(param.keep[,7]), 1.25*max(param.keep[,7])), xaxt="n", yaxt="n")
axis(2,las=1, at=c(min(param.keep[,7]), max(param.keep[,7])),
     labels=c(round(min(param.keep[,7]), 3), round(max(param.keep[,7]), 3)), cex.axis=1.5)
axis(1,las=1, at=c(min(param.keep[,1]), max(param.keep[,1])),
     labels=c(round(min(param.keep[,1]), 3), round(max(param.keep[,1]), 3)), cex.axis=1.5)
plot(param.keep[,2], param.keep[,7], xlim=c(0.25*min(param.keep[,2]), 1.25*max(param.keep[,2])),
     ylim=c(0.25*min(param.keep[,7]), 1.25*max(param.keep[,7])), xaxt="n", yaxt="n")
axis(1,las=1, at=c(min(param.keep[,2]), max(param.keep[,2])),
     labels=c(round(min(param.keep[,2]), 3), round(max(param.keep[,2]), 3)), cex.axis=1.5)
plot(param.keep[,3], param.keep[,7], xlim=c(0.25*min(param.keep[,3]), 1.25*max(param.keep[,3])),
     ylim=c(0.25*min(param.keep[,7]), 1.25*max(param.keep[,7])), xaxt="n", yaxt="n")
axis(1,las=1, at=c(min(param.keep[,3]), max(param.keep[,3])),
     labels=c(round(min(param.keep[,3]), 3), round(max(param.keep[,3]), 3)), cex.axis=1.5)
plot(param.keep[,4], param.keep[,7], xlim=c(0.25*min(param.keep[,4]), 1.25*max(param.keep[,4])),
     ylim=c(0.25*min(param.keep[,7]), 1.25*max(param.keep[,7])), xaxt="n", yaxt="n")
axis(1,las=1, at=c(min(param.keep[,4]), max(param.keep[,4])),
     labels=c(round(min(param.keep[,4]), 3), round(max(param.keep[,4]), 3)), cex.axis=1.5)
plot(param.keep[,5], param.keep[,7], xlim=c(0.25*min(param.keep[,5]), 1.25*max(param.keep[,5])),
     ylim=c(0.25*min(param.keep[,7]), 1.25*max(param.keep[,7])), xaxt="n", yaxt="n")
axis(1,las=1, at=c(min(param.keep[,5]), max(param.keep[,5])),
     labels=c(round(min(param.keep[,5]), 3), round(max(param.keep[,5]), 3)), cex.axis=1.5)
plot(param.keep[,6], param.keep[,7], xlim=c(0.25*min(param.keep[,6]), 1.25*max(param.keep[,6])),
     ylim=c(0.25*min(param.keep[,7]), 1.25*max(param.keep[,7])), xaxt="n", yaxt="n")
axis(1,las=1, at=c(5E-7, 2E-5),
     labels=c("5E-7", "2E-5"), cex.axis=1.5)
plot(0,0, type="n", tck=0, xlab="", ylab="", xaxt="n",yaxt="n")
text(0,0,names[7], cex=1.5, font=2)



#bind all together
varNDVI_100 = (perc.all.NDVI_100$NDVI[2:5]/100)*perc.all.NDVI_100$NDVI[9]
varNEE_100 = (perc.all.NEE_100$NEE[2:5]/100)*perc.all.NEE_100$NEE[9]
varNDVI_90 = (perc.all.NDVI_90$NDVI[2:5]/100)*perc.all.NDVI_90$NDVI[9]
varNEE_90 = (perc.all.NEE_90$NEE[2:5]/100)*perc.all.NEE_90$NEE[9]
varNDVI_70 = (perc.all.NDVI_70$NDVI[2:5]/100)*perc.all.NDVI_70$NDVI[9]
varNEE_70 = (perc.all.NEE_70$NEE[2:5]/100)*perc.all.NEE_70$NEE[9]
varNDVI_50 = (perc.all.NDVI_50$NDVI[2:5]/100)*perc.all.NDVI_50$NDVI[9]
varNEE_50 = (perc.all.NEE_50$NEE[2:5]/100)*perc.all.NEE_50$NEE[9]
varNDVI_20 = (perc.all.NDVI_20$NDVI[2:5]/100)*perc.all.NDVI_20$NDVI[9]
varNEE_20 = (perc.all.NEE_20$NEE[2:5]/100)*perc.all.NEE_20$NEE[9]

NDVI_var = c(varNDVI_all, varNDVI_90, varNDVI_70, varNDVI_50, varNDVI_20)
NEE_var = c(varNEE_all, varNEE_90, varNEE_70, varNEE_50, varNEE_20)
parameter = rep(rownames(perc.all.NEE_all)[2:5], 5)
range = rep(c(100,90,70,50,20), c(4,4,4,4,4))

var_all = data.frame(parameter=parameter, range=range, NEE=NEE_var, NDVI=NDVI_var)
head(var_all)

regNDVI_LitterRate = lm(log(NDVI)~range,data=var_all[var_all$parameter=="LitterRate",])
regNEE_LitterRate = lm(log(NEE)~range,data=var_all[var_all$parameter=="LitterRate",])

regNDVI_UptakeRate = lm(log(NDVI)~range, data=var_all[var_all$parameter=="UptakeRate",])
regNEE_UptakeRate = lm(log(NEE)~range, data=var_all[var_all$parameter=="UptakeRate",])

regNDVI_propN_fol = lm(log(NDVI)~range, data=var_all[var_all$parameter=="propN_fol",])
regNEE_propN_fol = lm(log(NEE)~range, data=var_all[var_all$parameter=="propN_fol",])

regNDVI_propN_roots = lm(log(NDVI)~range, data=var_all[var_all$parameter=="propN_roots",])
regNEE_propN_roots = lm(log(NEE)~range, data=var_all[var_all$parameter=="propN_roots",])

summary(regNDVI_LitterRate)
summary(regNEE_LitterRate)
summary(regNDVI_UptakeRate)
summary(regNEE_UptakeRate)
summary(regNDVI_propN_fol)
summary(regNEE_propN_fol)
summary(regNDVI_propN_roots)
summary(regNEE_propN_roots)


par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(var_all$NEE[var_all$parameter=="LitterRate"]~var_all$range[var_all$parameter=="LitterRate"], pch=16, cex=1, col="aquamarine", ylim=c(min(var_all$NEE)-5, max(var_all$NEE)+5))
lines(var_all$range[var_all$parameter=="LitterRate"], exp(predict(regNEE_LitterRate,list(range=var_all$range[var_all$parameter=="LitterRate"]))), col="aquamarine", lwd=2)
points(var_all$NEE[var_all$parameter=="UptakeRate"]~var_all$range[var_all$parameter=="UptakeRate"], pch=16, cex=1, col="darkgreen")
lines(var_all$range[var_all$parameter=="UptakeRate"], exp(predict(regNEE_UptakeRate,list(range=var_all$range[var_all$parameter=="UptakeRate"]))), col="darkgreen", lwd=2)
points(var_all$NEE[var_all$parameter=="propN_fol"]~var_all$range[var_all$parameter=="propN_fol"], pch=16, cex=1, col="mediumseagreen")
lines(var_all$range[var_all$parameter=="propN_fol"], exp(predict(regNEE_propN_fol,list(range=var_all$range[var_all$parameter=="propN_fol"]))), col="mediumseagreen", lwd=2)
points(var_all$NEE[var_all$parameter=="propN_roots"]~var_all$range[var_all$parameter=="propN_roots"], pch=16, cex=1, col="palegreen")
lines(var_all$range[var_all$parameter=="propN_roots"], exp(predict(regNEE_propN_roots,list(range=var_all$range[var_all$parameter=="propN_roots"]))), col="palegreen", lwd=2)

plot(var_all$NDVI[var_all$parameter=="LitterRate"]~var_all$range[var_all$parameter=="LitterRate"], pch=16, cex=1, col="aquamarine", ylim=c(min(var_all$NDVI), max(var_all$NDVI)))
lines(var_all$range[var_all$parameter=="LitterRate"], exp(predict(regNDVI_LitterRate,list(range=var_all$range[var_all$parameter=="LitterRate"]))), col="aquamarine", lwd=2)
points(var_all$NDVI[var_all$parameter=="UptakeRate"]~var_all$range[var_all$parameter=="UptakeRate"], pch=16, cex=1, col="darkgreen")
lines(var_all$range[var_all$parameter=="UptakeRate"], exp(predict(regNDVI_UptakeRate,list(range=var_all$range[var_all$parameter=="UptakeRate"]))), col="darkgreen", lwd=2)
points(var_all$NDVI[var_all$parameter=="propN_fol"]~var_all$range[var_all$parameter=="propN_fol"], pch=16, cex=1, col="mediumseagreen")
lines(var_all$range[var_all$parameter=="propN_fol"], exp(predict(regNDVI_propN_fol,list(range=var_all$range[var_all$parameter=="propN_fol"]))), col="mediumseagreen", lwd=2)
points(var_all$NDVI[var_all$parameter=="propN_roots"]~var_all$range[var_all$parameter=="propN_roots"], pch=16, cex=1, col="palegreen")
lines(var_all$range[var_all$parameter=="propN_roots"], exp(predict(regNDVI_propN_roots,list(range=var_all$range[var_all$parameter=="propN_roots"]))), col="palegreen", lwd=2)


save.image(file="PLOTS_061516_annualvardecomp.Rdata")




