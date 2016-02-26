#PLOTS FOR MANUSCRIPT 1

##################Histograms########################
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(density(param.keep[,1]), main="", ylab="Density", xlab=names(params[1]))
abline(v=param.best[1], col="red", lwd=3)
plot(density(param.keep[,2]), main="", ylab="Density", xlab=names(params[2]))
abline(v=param.best[2], col="red", lwd=3)
plot(density(param.keep[,3]), main="", ylab="Density", xlab=names(params[3]))
abline(v=param.best[3], col="red", lwd=3)
plot(density(param.keep[,4]), main="", ylab="Density", xlab=names(params[4]))
abline(v=param.best[4], col="red", lwd=3)
plot(density(param.keep[,5]), main="", ylab="Density", xlab=names(params[5]))
abline(v=param.best[5], col="red", lwd=3)
plot(density(param.keep[,6]), main="", ylab="Density", xlab=names(params[6]))
abline(v=param.best[6], col="red", lwd=3)
plot(density(param.keep[,7]), main="", ylab="Density", xlab=names(params[7]))
abline(v=param.best[7], col="red", lwd=3)
plot(density(param.keep[,8]), main="", ylab="Density", xlab=names(params[8]))
abline(v=param.best[8], col="red", lwd=3)




###################Temporal Validation#################
#load data
data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = data.assim[data.assim$Year == c(2009,2011,2013),]
data.sigma = data.sigma[data.sigma$Year  == c(2009,2011,2013),]
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
out=data.frame(solvemodel(param.best)) #with columns to match data.assim
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,9]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,9]))
head(data.compare1)
head(sigma.obs1)
upper=data.compare1+sigma.obs1
lower=data.compare1-sigma.obs1
NEE_dat = data.frame(Time=data.compare1$time, NEE=data.compare1$NEE, lower=lower$NEE, upper=upper$NEE)
head(NEE_dat)
NEE_dat=NEE_dat[!is.na(NEE_dat$NEE),]
head(NEE_dat)
NEE_dat = data.frame(Time=data.compare1$time, NEE=data.compare1$NEE, lower=lower$NEE, upper=upper$NEE)
head(NEE_dat)
NEE_dat=NEE_dat[!is.na(NEE_dat$NEE),]
head(NEE_dat)
NDVI_dat = data.frame(Time=data.compare1$time, NDVI=data.compare1$NDVI, lower=lower$NDVI, upper=upper$NDVI)
head(NDVI_dat)
NDVI_dat=NDVI_dat[!is.na(NDVI_dat$NDVI),]
head(NDVI_dat)
NDVI_dat = data.frame(Time=data.compare1$time, NDVI=data.compare1$NDVI, lower=lower$NDVI, upper=upper$NDVI)
head(NDVI_dat)
NDVI_dat=NDVI_dat[!is.na(NDVI_dat$NDVI),]
head(NDVI_dat)

#create averaged data for spin-up
Temp.spin= tapply(data$Temp_ARF, data$DOY, mean)
plot(Temp.spin)
Temp_GS = data$Temp_ARF[data$DOY>=data$startDOY[1] & data$DOY <= data$endDOY[1]]
AvgTempGS = mean(Temp_GS)
TempAvg.spin = rep(AvgTempGS, 366)
PAR.spin = tapply(data$PAR_ARF, data$DOY, mean)
plot(PAR.spin)
DOY.spin=seq(1:366)
Year.spin=rep(2000,366)
data.spin=data.frame(Year=Year.spin, DOY=DOY.spin, Temp=Temp.spin, Temp_avg=TempAvg.spin, PAR=PAR.spin)
head(data.spin)

#seasonality scalar
sen.day=min(data.spin$DOY[which(data.spin$Temp<=10 & data.spin$DOY>200)])
sen.day #DOY of senescence 
num.days = 366
senDOY = rep(sen.day, num.days)
data.spin = data.frame(data.spin, senDOY = senDOY)
start.day=min(data.spin$DOY[which(data.spin$Temp>=-5 & data.spin$DOY>120)])
start.day #start day
startDOY = rep(start.day, num.days)
data.spin = data.frame(data.spin, startDOY = startDOY)
end.day=min(data.spin$DOY[which(data.spin$Temp<=0 & data.spin$DOY>240)])
end.day #end day
endDOY = rep(end.day, num.days)
data.spin = data.frame(data.spin, endDOY = endDOY)
head(data.spin)


#create scalar
scal.seas.spin=rep(1, length(data.spin$DOY))
for (i in 1:length(data.spin$DOY)){
  if(data.spin$DOY[i]<data.spin$startDOY[i]){ #prior to snow melt
    scal.seas.spin[i]=0
  }
  if(data.spin$DOY[i]>=data.spin$startDOY[i]){ #after melt
    if(data.spin$DOY[i]<=data.spin$senDOY[i]){ #prior to peak
      slope = 1/(data.spin$senDOY[i]-data.spin$startDOY[i])
      scal.seas.spin[i] = 0+(slope*(data.spin$DOY[i]-data.spin$startDOY[i]))
    }
    if(data.spin$DOY[i]>data.spin$senDOY[i] & data.spin$DOY[i]<data.spin$endDOY[i]){ #after peak but before frost
      slope = 1/(data.spin$endDOY[i]-data.spin$senDOY[i])
      scal.seas.spin[i] = 0+(slope*(data.spin$endDOY[i]-data.spin$DOY[i]))
    }
    if(data.spin$DOY[i]>=data.spin$endDOY[i]){ #after frost
      scal.seas.spin[i]=0
    }
  }
}


#temperature scalar
Tmax = max(data.spin$Temp)
Tmin = min(data.spin$Temp)
scal.temp.spin=NULL
for (i in 1:length(data.spin$Temp)){
  scal.temp.spin[i] = (data.spin$Temp[i] - Tmin)/(Tmax-Tmin) 
}


#run model spin up for current parameter set
numyears = 50
Year.spin = rep(data.spin$Year, numyears)
DOY.spin = rep(data.spin$DOY, numyears)
Temp.spin = rep(data.spin$Temp, numyears)
TempAvg.spin = rep(data.spin$Temp_avg, numyears)
PAR.spin = rep(data.spin$PAR, numyears)
scal.temp.spin1 = rep(scal.temp.spin, numyears)
scal.seas.spin1 = rep(scal.seas.spin, numyears)

time = seq(1:length(DOY.spin))

#Step 4: make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=Temp.spin, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin1, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=scal.seas.spin1, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)


#OPEN 3_Model.R and run it the first time
out.spin= data.frame(solvemodel(param.best, state)) #creates table of model output
plot(out.spin$Biomass_C)
end.time = length(out.spin[,1])
#adjust starting values
state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
            Biomass_N = out.spin$Biomass_N[end.time], 
            SOM_C = out.spin$SOM_C[end.time], 
            SOM_N = out.spin$SOM_N[end.time],
            Available_N = out.spin$Available_N[end.time])
state.best=state

time = seq(1:length(data$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)


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
sensvars = c("NEE",
             "NDVI")

s.global <- sensRange(func=solvemodel, parms=param.best, state=state, sensvar = sensvars, parInput=param.keep)
s.global.summ = summary(s.global) #create summary table
head(s.global.summ) #view first 6 rows
tail(s.global.summ)

#get model output & confidence intervals organized
out=data.frame(solvemodel(param.best, state))
NEE_summ = data.frame(Time=s.global.summ[1:2191,1], NEE=out$NEE, sd=s.global.summ[1:2191,3], q05=s.global.summ[1:2191,6], q95=s.global.summ[1:2191,10], q25=s.global.summ[1:2191,7], q75=s.global.summ[1:2191,9])
head(NEE_summ)
NDVI_summ = data.frame(Time=s.global.summ[2192:4382,1], NDVI=out$NDVI, sd=s.global.summ[2192:4382,3], q05=s.global.summ[2192:4382,6], q95=s.global.summ[2192:4382,10], q25=s.global.summ[2192:4382,7], q75=s.global.summ[2192:4382,9])
head(NDVI_summ)

#get data ready
data.compare=read.csv("Assimilation_data_ALL.csv")
data.compare1=data.compare[data.compare$Year== c(2009,2011,2013),]
data.compare2=data.compare[data.compare$Year== c(2010,2012),]
data.compare_NEE1=data.compare1[complete.cases(data.compare1[,6]),c(1:5,6)]
data.compare_NEE2=data.compare2[complete.cases(data.compare2[,6]),c(1:5,6)]
data.compare_NDVI1=data.compare1[complete.cases(data.compare1[,9]),c(1:5,9)]
data.compare_NDVI2=data.compare2[complete.cases(data.compare2[,9]),c(1:5,9)]
out.compare_NEE2 = out[match(data.compare_NEE2$Time, out$time),]
out.compare_NDVI2 = out[match(data.compare_NDVI2$Time, out$time),]


#linear regressions for years not assimilated
reg_NEE = lm(out.compare_NEE2$NEE~data.compare_NEE2$NEE)
reg_NDVI = lm(out.compare_NDVI2$NDVI~data.compare_NDVI2$NDVI)


#plot
par(mar=c(4,4,2,2))
#layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 4, 3, byrow = TRUE), widths=c(3,1,1))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(3,1))

plot(NEE~Time,data=NEE_summ, type="p", pch=16, cex=0.5, axes=FALSE, xlab="", ylim=c(-5,3))
axis(1, at=c(0,365,730,1096,1461,1826,2191), labels=c("","","","","","",""))
mtext("2009", side=1, at=200) 
mtext("2010", side=1, at=550) 
mtext("2011", side=1, at=925) 
mtext("2012", side=1, at=1275) 
mtext("2013", side=1, at=1650) 
mtext("2014", side=1, at=2010) 
axis(2)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
with(NEE_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = "grey75", border = FALSE))
points(NEE~Time, data=NEE_summ, pch=16, cex=0.75)
points(NEE~Time, data=data.compare_NEE1, pch=16, col="gray29", cex=0.75)
points(NEE~Time, data=data.compare_NEE2, pch=16, col="blue", cex=0.75)
legend("topleft", legend=c("CCaN NEE", "Assimilated NEE Measurements", "NEE Measurements Not Assimilated"), bty="n", pch=16, col=c("black", "gray29", "blue"))

plot(out.compare_NEE2$NEE~data.compare_NEE2$NEE, pch=16, cex=0.75, ylab="CCaN NEE", xlab="Measured NEE", ylim=c(-4,2), xlim=c(-4,2))
abline(0,1, col="red", lty=2, lwd=2)


plot(NDVI~Time,data=NDVI_summ, type="p", pch=16, cex=0.5, axes=FALSE, xlab="",ylim=c(0,1))
axis(1, at=c(0,365,730,1096,1461,1826,2191), labels=c("","","","","","",""))
mtext("2009", side=1, at=200) 
mtext("2010", side=1, at=550) 
mtext("2011", side=1, at=925) 
mtext("2012", side=1, at=1275) 
mtext("2013", side=1, at=1650) 
mtext("2014", side=1, at=2010) 
axis(2)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
with(NDVI_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = "grey75", border = FALSE))
points(NDVI~Time, data=NDVI_summ, pch=16, cex=0.75)
points(NDVI_ASDscal~Time, data=data.compare_NDVI1, pch=16, col="gray29", cex=0.75)
points(NDVI_ASDscal~Time, data=data.compare_NDVI2, pch=16, col="blue", cex=0.75)
legend("topleft", legend=c("CCaN NDVI", "Assimilated NDVI Measurements", "NDVI Measurements Not Assimilated"), bty="n", pch=16, col=c("black", "gray29", "blue"))

plot(out.compare_NDVI2$NDVI~data.compare_NDVI2$NDVI_ASDscal, pch=16, cex=0.75, ylab="CCaN NDVI", xlab="Measured NDVI", ylim=c(0,1), xlim=c(0,1))
abline(0,1, col="red", lty=2, lwd=2)




################Spatial Validation######################

dat = data.frame(read.csv("Summary_AllSites.csv")) #select data file
head(dat) #fiew first 6 rows


#put it all into a table
summary = data.frame(matrix(1, 1, 15))
colnames(summary) = c("Latitude", "Biomass_C", "Biomass_N", "SOM_C", "SOM_N", "Available_N", "CCaN_max", "CCaN.MODIS_max", "MODIS_max", "CCaN_avg", "CCaN.MODIS_avg", "MODIS_avg", "Tmax", "Tavg", "PARavg")
head(summary)


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
  
  ###################MODEL SPINUP######################
  numyears = 50
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
  
  state  <- c(Biomass_C = 400, 
              Biomass_N = 7.5, 
              SOM_C = 9000, 
              SOM_N = 257,
              Available_N = 1)
  
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
  CCaN_max = max(out$NDVI)
  CCaN.MODIS_max = max(out$NDVI_MODIS)
  MODIS_max = max(data$NDVI)
  
  #determine GS average NDVI 
  #pull out GS data (DOY > 140 & DOY < 250)
  CCaN_gsNDVI = out$NDVI[out$DOY>=140 & out$DOY <=250]
  CCaN_gsNDVI.MOD = out$NDVI_MODIS[out$DOY>=140 & out$DOY <=250]
  MODIS_gsNDVI = data$NDVI.avg[data$DOY>=140 & data$DOY <=250]
  #calculate average
  CCaN_avg = mean(CCaN_gsNDVI)
  CCaN.MODIS_avg = mean(CCaN_gsNDVI.MOD)
  MODIS_avg = mean(MODIS_gsNDVI)
  
  #determine GS Avg Temp & PAR 
  #pull out GS data (DOY > 140 & DOY < 250)
  Temp_GS = data$LST.avg[data$DOY>=140 & data$DOY <=250]
  PAR_GS = data$PAR.avg[data$DOY>=140 & data$DOY <=250]
  #calculate average
  Tavg = mean(Temp_GS)
  PARavg = mean(PAR_GS)
  
  summary = rbind(summary, c(lat.i, state, CCaN_max, CCaN.MODIS_max, MODIS_max, CCaN_avg, CCaN.MODIS_avg, MODIS_avg, Tmax, Tavg, PARavg)) #bind new row to table
}


summary=summary[-1,]
write.csv(summary, "CaTT_Summary_temp") #save CSV 

#regressions with latitude - NDVI
par(mfrow=c(1,1))
plot(summary$CCaN.MODIS_avg~summary$Latitude, pch=16, xlab="Latitude", ylab="CCaN NDVI (Adjusted)", ylim=c(0.1,0.6))

par(fig=c(0.6, 1, 0.5, 1), new = T)
plot(summary$CCaN.MODIS_avg~summary$MODIS_avg, ylab="CCaN NDVI (Adjusted)", xlab="MODIS NDVI", ylim=c(0.1,0.7), xlim=c(0.1,0.7))
abline(0,1, col="red", lty=2, lwd=2)


#modelled vs. measured
reg_NDVI = lm(summary$CCaN.MODIS_avg~summary$MODIS_avg)
summary(reg_NDVI)

#regressions with temperature - NDVI
reg_MODIS.temp = lm(summary$MODIS_avg~summary$Tavg)
reg_CCaN.temp = lm(summary$CCaN_avg~summary$Tavg)

#regression stats
summary(reg_MODIS.temp) #slope is sensitivity
summary(reg_CCaN.temp) #slope is sensitivity



###########Temperature Sensitivity Comparison################
Sample = c("CCaN", "LTER", "Goetz", "MODIS")
Sensitivty = c(0.027, 0.022, 0.02, 0.033)
par(mfrow=c(1,1))
barplot(Sensitivty, names.arg=Sample, ylab="Temperatre Sensitivity")



##########################VARIANCE DECOMPOSITION ANALYSIS#######################

head(param.keep) #view table of accepted parameters
summarytable
means



#to perform the variance decomposition analysis, you need to:
# 1) alter each parameter individually holding all other parameters constant at their means
# 2) run the model for each parameter set to obtain an ensemble of model runs
# 3) for each model run, calculate the monthly average of the output
# 4) Calculate the variance in monthly averages for each parameter - this gives you the contribution of that parameter to the model variance

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
MVar_cue = data.frame(matrix(1,1,11))
colnames(MVar_cue)=c("Month", colnames(out[,2:11]))
MVar_BiomassCN = data.frame(matrix(1,1,11))
colnames(MVar_BiomassCN)=c("Month", colnames(out[,2:11]))


#need to create a vector of months to append to model output
months = rep(c(seq(1:12)),
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
months.leap = rep(c(seq(1:12)),
                  c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

months = c(months, months, months, months.leap, months, months)

state.best
state=state.best


#kplant
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_kplant)
  MVar_kplant = rbind(MVar_kplant, monthly.avg)
}  

#LitterRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_LitterRate)
  MVar_LitterRate = rbind(MVar_LitterRate, monthly.avg)
}  

#UptakeRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_UptakeRate)
  MVar_UptakeRate = rbind(MVar_UptakeRate, monthly.avg)
}  

#propN_fol
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_fol)
  MVar_propN_fol = rbind(MVar_propN_fol, monthly.avg)
}  

#propN_roots
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_roots)
  MVar_propN_roots = rbind(MVar_propN_roots, monthly.avg)
}  

#netNrate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_netNrate)
  MVar_netNrate = rbind(MVar_netNrate, monthly.avg)
}  

#cue
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_cue)
  MVar_cue = rbind(MVar_cue, monthly.avg)
}  

#BiomassCN
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_BiomassCN)
  MVar_BiomassCN = rbind(MVar_BiomassCN, monthly.avg)
}


MVar_kplant = MVar_kplant[-1,]
MVar_LitterRate = MVar_LitterRate[-1,]
MVar_UptakeRate = MVar_UptakeRate[-1,]
MVar_propN_fol = MVar_propN_fol[-1,]
MVar_propN_roots = MVar_propN_roots[-1,]
MVar_netNrate = MVar_netNrate[-1,]
MVar_cue = MVar_cue[-1,]
MVar_BiomassCN = MVar_BiomassCN[-1,]
MVar_BiomassC = MVar_BiomassC[-1,]
MVar_BiomassN = MVar_BiomassN[-1,]
MVar_SOMC = MVar_SOMC[-1,]
MVar_SOMN = MVar_SOMN[-1,]
MVar_AvailN = MVar_AvailN[-1,]

var.kplant = aggregate(MVar_kplant[,2:11], list(MVar_kplant$Month), var)
var.LitterRate = aggregate(MVar_LitterRate[,2:11], list(MVar_LitterRate$Month), var)
var.UptakeRate = aggregate(MVar_UptakeRate[,2:11], list(MVar_UptakeRate$Month), var)
var.propN_fol = aggregate(MVar_propN_fol[,2:11], list(MVar_propN_fol$Month), var)
var.propN_roots = aggregate(MVar_propN_roots[,2:11], list(MVar_propN_roots$Month), var)
var.netNrate = aggregate(MVar_netNrate[,2:11], list(MVar_netNrate$Month), var)
var.cue = aggregate(MVar_cue[,2:11], list(MVar_cue$Month), var)
var.BiomassCN = aggregate(MVar_BiomassCN[,2:11], list(MVar_BiomassCN$Month), var)
var.BiomassC = aggregate(MVar_BiomassC[,2:11], list(MVar_BiomassC$Month), var)
var.BiomassN = aggregate(MVar_BiomassN[,2:11], list(MVar_BiomassN$Month), var)
var.SOMC = aggregate(MVar_SOMC[,2:11], list(MVar_SOMC$Month), var)
var.SOMN = aggregate(MVar_SOMN[,2:11], list(MVar_SOMN$Month), var)
var.AvailN = aggregate(MVar_AvailN[,2:11], list(MVar_AvailN$Month), var)

parameters = rep(names(params), c(12,12,12,12,12,12,12,12,12,12,12,12,12))

all = rbind(var.kplant, var.LitterRate, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.netNrate, var.cue, var.BiomassCN, var.BiomassC, var.BiomassN,
            var.SOMC, var.SOMN, var.AvailN)

all=cbind(Parameters = parameters, all)

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

perc.cue = (var.cue[,2:11]/var.total[,2:11])*100
perc.cue = cbind(Parameter = rep("cue", 12), Month=var.total$Group.1, perc.cue)

perc.BiomassCN = (var.BiomassCN[,2:11]/var.total[,2:11])*100
perc.BiomassCN = cbind(Parameter = rep("BiomassCN", 12), Month=var.total$Group.1, perc.BiomassCN)

perc.BiomassC = (var.BiomassC[,2:11]/var.total[,2:11])*100
perc.BiomassC = cbind(Parameter = rep("BiomassC", 12), Month=var.total$Group.1, perc.BiomassC)

perc.BiomassN = (var.BiomassN[,2:11]/var.total[,2:11])*100
perc.BiomassN = cbind(Parameter = rep("BiomassN", 12), Month=var.total$Group.1, perc.BiomassN)

perc.SOMC = (var.SOMC[,2:11]/var.total[,2:11])*100
perc.SOMC = cbind(Parameter = rep("SOMC", 12), Month=var.total$Group.1, perc.SOMC)

perc.SOMN = (var.SOMN[,2:11]/var.total[,2:11])*100
perc.SOMN = cbind(Parameter = rep("SOMN", 12), Month=var.total$Group.1, perc.SOMN)

perc.AvailN = (var.AvailN[,2:11]/var.total[,2:11])*100
perc.AvailN = cbind(Parameter = rep("AvailN", 12), Month=var.total$Group.1, perc.AvailN)


#create a table binding all together

perc.all = rbind(perc.kplant, perc.LitterRate, perc.UptakeRate, 
                 perc.propN_fol, perc.propN_roots, perc.netNrate, perc.cue, perc.BiomassCN,
                 perc.BiomassC, perc.BiomassN, perc.SOMC, perc.SOMN, perc.AvailN)
perc.all = perc.all[,-11]
head(perc.all)
tail(perc.all)

####barplots####

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 3:11) { #for each output
  sub = perc.all[,c(1,2,n)]
  sub1 = table(sub$Parameter, sub$Month)
  sub1[1,] = sub[1:12,3]
  sub1[2,] = sub[13:24,3]
  sub1[3,] = sub[25:36,3]
  sub1[4,] = sub[37:48,3]
  sub1[5,] = sub[49:60,3]
  sub1[6,] = sub[61:72,3]
  sub1[7,] = sub[73:84,3]
  sub1[8,] = sub[85:96,3]
  sub1[9,] = sub[97:108,3]
  sub1[10,] = sub[109:120,3]
  sub1[11,] = sub[121:132,3]
  sub1[12,] = sub[133:144,3]
  sub1[13,] = sub[145:156,3]
  barplot(sub1, col=c("darkolivegreen3", "aquamarine", "darkgreen", "mediumseagreen",
                      "palegreen", "darkblue", "lightskyblue", "maroon4", "gray87", "azure2", 
                      "gray29", "gray57", "darkslategray"),             
          main=names(perc.all[n]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100)) #plot the data
} #end of for loop


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
sub1[7,] = sub[73:84,3]
sub1[8,] = sub[85:96,3]
sub1[9,] = sub[97:108,3]
sub1[10,] = sub[109:120,3]
sub1[11,] = sub[121:132,3]
sub1[12,] = sub[133:144,3]
sub1[13,] = sub[145:156,3]
barplot(sub1, col=c("darkolivegreen3", "aquamarine", "darkgreen", "mediumseagreen",
                    "palegreen", "darkblue", "lightskyblue", "maroon4", "gray87", "azure2", 
                    "gray29", "gray57", "darkslategray"),           
        main=names(perc.all[11]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100), legend=TRUE) #plot the data


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
AVar_cue = data.frame(matrix(1,1,3))
colnames(AVar_cue)=colnames(out[,7:9])
AVar_BiomassCN = data.frame(matrix(1,1,3))
colnames(AVar_BiomassCN)=colnames(out[,7:9])
AVar_BiomassC = data.frame(matrix(1,1,3))
colnames(AVar_BiomassC)=colnames(out[,7:9])
AVar_BiomassN = data.frame(matrix(1,1,3))
colnames(AVar_BiomassN)=colnames(out[,7:9])
AVar_SOMC = data.frame(matrix(1,1,3))
colnames(AVar_SOMC)=colnames(out[,7:9])
AVar_SOMN = data.frame(matrix(1,1,3))
colnames(AVar_SOMN)=colnames(out[,7:9])
AVar_AvailN = data.frame(matrix(1,1,3))
colnames(AVar_AvailN)=colnames(out[,7:9])

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
AVar_cue_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_cue_NDVI)=c("NDVI")
AVar_BiomassCN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_BiomassCN_NDVI)=c("NDVI")
AVar_BiomassC_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_BiomassC_NDVI)=c("NDVI")
AVar_BiomassN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_BiomassN_NDVI)=c("NDVI")
AVar_SOMC_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_SOMC_NDVI)=c("NDVI")
AVar_SOMN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_SOMN_NDVI)=c("NDVI")
AVar_AvailN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_AvailN_NDVI)=c("NDVI")

first=4
second=6

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

#cue
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$cue>=summarytable[7,first] & param.keep_NEE_NDVI_UNBdata$cue<=summarytable[7,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_cue) #change names
  AVar_cue = rbind(AVar_cue, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_cue_NDVI) #change names
  AVar_cue_NDVI = rbind(AVar_cue_NDVI, annual.avgNDVI) #add row to table
}  

#BiomassCN
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$BiomassCN>=summarytable[8,first] & param.keep_NEE_NDVI_UNBdata$BiomassCN<=summarytable[8,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_BiomassCN) #change names
  AVar_BiomassCN = rbind(AVar_BiomassCN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_BiomassCN_NDVI) #change names
  AVar_BiomassCN_NDVI = rbind(AVar_BiomassCN_NDVI, annual.avgNDVI) #add row to table
} 

#Biomass_C
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$Biomass_C>=summarytable[9,first] & param.keep_NEE_NDVI_UNBdata$Biomass_C<=summarytable[9,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_BiomassC) #change names
  AVar_BiomassC = rbind(AVar_BiomassC, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_BiomassC_NDVI) #change names
  AVar_BiomassC_NDVI = rbind(AVar_BiomassC_NDVI, annual.avgNDVI) #add row to table
} 

#Biomass_N
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$Biomass_N>=summarytable[10,first] & param.keep_NEE_NDVI_UNBdata$Biomass_N<=summarytable[10,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[10] = unlist(c(param.keep[i,10]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_BiomassN) #change names
  AVar_BiomassN = rbind(AVar_BiomassN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_BiomassN_NDVI) #change names
  AVar_BiomassN_NDVI = rbind(AVar_BiomassN_NDVI, annual.avgNDVI) #add row to table
} 

#SOM_C
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$SOM_C>=summarytable[11,first] & param.keep_NEE_NDVI_UNBdata$SOM_C<=summarytable[11,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[11] = unlist(c(param.keep[i,11]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_SOMC) #change names
  AVar_SOMC = rbind(AVar_SOMC, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_SOMC_NDVI) #change names
  AVar_SOMC_NDVI = rbind(AVar_SOMC_NDVI, annual.avgNDVI) #add row to table
} 

#SOM_N
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$SOM_N>=summarytable[12,first] & param.keep_NEE_NDVI_UNBdata$SOM_N<=summarytable[12,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[12] = unlist(c(param.keep[i,12]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_SOMN) #change names
  AVar_SOMN = rbind(AVar_SOMN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_SOMN_NDVI) #change names
  AVar_SOMN_NDVI = rbind(AVar_SOMN_NDVI, annual.avgNDVI) #add row to table
} 

#Available_N
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$Available_N>=summarytable[13,first] & param.keep_NEE_NDVI_UNBdata$Available_N<=summarytable[13,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[13] = unlist(c(param.keep[i,13]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i,state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_AvailN) #change names
  AVar_AvailN = rbind(AVar_AvailN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_AvailN_NDVI) #change names
  AVar_AvailN_NDVI = rbind(AVar_AvailN_NDVI, annual.avgNDVI) #add row to table
} 


AVar_kplant = AVar_kplant[-1,]
AVar_LitterRate = AVar_LitterRate[-1,]
AVar_UptakeRate = AVar_UptakeRate[-1,]
AVar_propN_fol = AVar_propN_fol[-1,]
AVar_propN_roots = AVar_propN_roots[-1,]
AVar_netNrate = AVar_netNrate[-1,]
AVar_cue = AVar_cue[-1,]
AVar_BiomassCN = AVar_BiomassCN[-1,]
AVar_BiomassC = AVar_BiomassC[-1,]
AVar_BiomassN = AVar_BiomassN[-1,]
AVar_SOMC = AVar_SOMC[-1,]
AVar_SOMN = AVar_SOMN[-1,]
AVar_AvailN = AVar_AvailN[-1,]

var.kplant = apply(AVar_kplant, 2, var)
var.LitterRate = apply(AVar_LitterRate, 2, var)
var.UptakeRate = apply(AVar_UptakeRate, 2, var)
var.propN_fol = apply(AVar_propN_fol, 2, var)
var.propN_roots = apply(AVar_propN_roots, 2, var)
var.netNrate = apply(AVar_netNrate, 2, var)
var.cue = apply(AVar_cue, 2, var)
var.BiomassCN = apply(AVar_BiomassCN, 2, var)
var.BiomassC = apply(AVar_BiomassC, 2, var)
var.BiomassN = apply(AVar_BiomassN, 2, var)
var.SOMC = apply(AVar_SOMC, 2, var)
var.SOMN = apply(AVar_SOMN, 2, var)
var.AvailN = apply(AVar_AvailN, 2, var)



AVar_kplant_NDVI = AVar_kplant_NDVI[-1,]
AVar_LitterRate_NDVI  = AVar_LitterRate_NDVI[-1,]
AVar_UptakeRate_NDVI  = AVar_UptakeRate_NDVI[-1,]
AVar_propN_fol_NDVI  = AVar_propN_fol_NDVI[-1,]
AVar_propN_roots_NDVI  = AVar_propN_roots_NDVI[-1,]
AVar_netNrate_NDVI  = AVar_netNrate_NDVI[-1,]
AVar_cue_NDVI  = AVar_cue_NDVI[-1,]
AVar_BiomassCN_NDVI  = AVar_BiomassCN_NDVI[-1,]
AVar_BiomassC_NDVI  = AVar_BiomassC_NDVI[-1,]
AVar_BiomassN_NDVI  = AVar_BiomassN_NDVI[-1,]
AVar_SOMC_NDVI  = AVar_SOMC_NDVI[-1,]
AVar_SOMN_NDVI  = AVar_SOMN_NDVI[-1,]
AVar_AvailN_NDVI  = AVar_AvailN_NDVI[-1,]

var.kplant_NDVI = var(AVar_kplant_NDVI)
var.LitterRate_NDVI = var(AVar_LitterRate_NDVI)
var.UptakeRate_NDVI = var(AVar_UptakeRate_NDVI)
var.propN_fol_NDVI = var(AVar_propN_fol_NDVI)
var.propN_roots_NDVI = var(AVar_propN_roots_NDVI)
var.netNrate_NDVI = var(AVar_netNrate_NDVI)
var.cue_NDVI = var(AVar_cue_NDVI)
var.BiomassCN_NDVI = var(AVar_BiomassCN_NDVI)
var.BiomassC_NDVI = var(AVar_BiomassC_NDVI)
var.BiomassN_NDVI = var(AVar_BiomassN_NDVI)
var.SOMC_NDVI = var(AVar_SOMC_NDVI)
var.SOMN_NDVI = var(AVar_SOMN_NDVI)
var.AvailN_NDVI = var(AVar_AvailN_NDVI)


all_NEE = rbind(var.kplant, var.LitterRate, var.UptakeRate, var.propN_fol, 
                var.propN_roots, var.netNrate, var.cue, var.BiomassCN, var.BiomassC, var.BiomassN, 
                var.SOMC, var.SOMN, var.AvailN)
all_NDVI = rbind(var.kplant_NDVI, var.LitterRate_NDVI, var.UptakeRate_NDVI, var.propN_fol_NDVI, 
                 var.propN_roots_NDVI, var.netNrate_NDVI, var.cue_NDVI, var.BiomassCN_NDVI, var.BiomassC_NDVI, var.BiomassN_NDVI, 
                 var.SOMC_NDVI, var.SOMN_NDVI, var.AvailN_NDVI)

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
perc.cue = (var.cue/var.total)*100
perc.BiomassCN = (var.BiomassCN/var.total)*100

perc.kplant_NDVI = (var.kplant_NDVI/var.total_NDVI)*100
perc.LitterRate_NDVI = (var.LitterRate_NDVI/var.total_NDVI)*100
perc.UptakeRate_NDVI = (var.UptakeRate_NDVI/var.total_NDVI)*100
perc.propN_fol_NDVI = (var.propN_fol_NDVI/var.total_NDVI)*100
perc.propN_roots_NDVI = (var.propN_roots_NDVI/var.total_NDVI)*100
perc.netNrate_NDVI = (var.netNrate_NDVI/var.total_NDVI)*100
perc.cue_NDVI = (var.cue_NDVI/var.total_NDVI)*100
perc.BiomassCN_NDVI = (var.BiomassCN_NDVI/var.total_NDVI)*100

#create a table binding all together

perc.all_NEE = rbind(perc.kplant, perc.LitterRate, perc.UptakeRate, 
                     perc.propN_fol, perc.propN_roots, perc.netNrate, perc.cue, perc.BiomassCN, var.total)

rownames(perc.all_NEE)=c(names(params[1:8]), "total")

perc.all_NDVI = rbind(perc.kplant_NDVI, perc.LitterRate_NDVI, perc.UptakeRate_NDVI, 
                      perc.propN_fol_NDVI, perc.propN_roots_NDVI, perc.netNrate_NDVI, perc.cue_NDVI, perc.BiomassCN_NDVI, var.total_NDVI)

rownames(perc.all_NDVI)=c(names(params[1:8]), "total")
colnames(perc.all_NDVI)=c("NDVI")



#store for this subset
perc.all.NEE_20 = data.frame(perc.all_NEE)
perc.all.NDVI_20 = data.frame(perc.all_NDVI)

####barplots####
barplot(perc.all_NEE, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "purple", 
                            "deepskyblue", "dodgerblue3", "forestgreen", "darkgray"),  legend=TRUE )
barplot(perc.all.NEE$NEE, names.arg=names(params[1:9]), cex.names=0.5, 
        col="forestgreen", horiz=TRUE, main="NEE") #plot the data

barplot(perc.all.NDVI$NDVI, names.arg=names(params[1:9]), cex.names=0.5, 
        col="forestgreen", horiz=TRUE, main="NDVI") #plot the data

save.image(file="Variance_121115.Rdata")




