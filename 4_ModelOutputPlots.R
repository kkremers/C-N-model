##########################PLOT MODEL OUTPUTS###########################

#create dataframe for model spinup

#Step 2: calculate decadal averages
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

plot(scal.seas.spin)

#temperature scalar
Tmax = max(data.spin$Temp)
Tmin = min(data.spin$Temp)
scal.temp.spin=NULL
for (i in 1:length(data.spin$Temp)){
  scal.temp.spin[i] = (data.spin$Temp[i] - Tmin)/(Tmax-Tmin) 
}

plot(scal.temp.spin, type="l")

#run model spin up for current parameter set
numyears = 25
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


state  <- c(Biomass_C = 715, 
            Biomass_N = 14, 
            SOM_C = 8700, 
            SOM_N = 355,
            Available_N = 0.01)

#OPEN 3_Model.R and run it the first time
out.spin= data.frame(solvemodel(params, state)) #creates table of model output
plot(out.spin$Biomass_N)
plot(out.spin$Biomass_C)
plot(out.spin$SOM_N)
plot(out.spin$SOM_C)
plot(out.spin$Available_N)
end.time = length(out.spin[,1])
#adjust starting values
state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
            Biomass_N =  out.spin$Biomass_N[end.time], 
            SOM_C = out.spin$SOM_C[end.time], 
            SOM_N = out.spin$SOM_N[end.time],
            Available_N = out.spin$Available_N[end.time])


time = seq(1:length(data$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)

out= data.frame(solvemodel(params, state)) #creates table of model output


#plot fluxes
plot(out$GPP)
plot(out$Ra)
plot(out$Rh)



ntrans.avg = tapply(out$Ntrans, out$DOY, mean)
plot(ntrans.avg)
sum(ntrans.avg[150:240])
sum(ntrans.avg[c(1:150,241:366)])



plot(out$GPP~out$LAI, xlab="LAI", ylab="GPP")
plot(out$LAI~out$Temp, xlab="Temp", ylab="LAI")



#plot pools
par(mfrow=c(3,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="forestgreen", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="forestgreen",  main = "Biomass N", xlab="", ylab="g N m-2")
plot(out$SOM_C~out$time, type="l", col="brown", main = "SOM C", xlab="", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="brown", main = "SOM N", xlab="Time (days)", ylab="g N m-2")
plot(out$Available_N~out$time, type="l", col="blue", main = "Available_ N", xlab="Time (days)", ylab="g N m-2",lty=2)

#see how well data matches
#to compare on 1:1 line with data, need to select only points for which data is available
data.compare2=read.csv("Assimilation_data_ALL.csv")
data.compare_NEE=data.compare2[complete.cases(data.compare2[,6]),c(1:5,6)]
head(data.compare_NEE)
data.compare_NDVI=data.compare2[complete.cases(data.compare2[,7]),c(1:5,7)]
head(data.compare_NDVI)
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]

#plot
par(mfrow=c(1,1), mar=c(2,2,2,2))
plot(NEE~Time,data=data.assim, type="p", pch=16, cex=0.25, ylim=c(-4,2))
arrows(data.assim$Time,data.assim$NEE+data.sigma$NEE, data.assim$Time, data.assim$NEE-data.sigma$NEE, angle=90, code=3, length=0.01, col="gray47")
points(NEE~Time, data=data.assim, pch=16, cex=0.4, col="blue")
abline(h=0)

plot(NDVI~Time,data=data.assim, type="p", pch=16, cex=0.25, ylim=c(0,1))
arrows(data.assim$Time,data.assim$NDVI+data.sigma$NDVI, data.assim$Time, data.assim$NDVI-data.sigma$NDVI, angle=90, code=3, length=0.01, col="gray47")
points(NDVI~Time, data=data.assim, pch=16, cex=0.4, col="blue")




#PLOT DATA FOR ALL YEARS
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE~out$time, col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE, out.compare_NEE$NEE, ylim=c(-4, 4), xlim=c(-4,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI~out$time, col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0,0.8))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_NDVI$NDVI, out.compare_NDVI$NDVI, ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1) )
abline(0,1, col="red")


#PLOT DATA FOR 2009
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2009]~out$time[out$year==2009], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2009], out.compare_NEE$NEE[out.compare_NEE$year==2009],ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")


plot(out$NDVI[out$year==2009]~out$time[out$year==2009], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2009], out.compare_NDVI$NDVI[out.compare_NDVI$year==2009], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2010
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2010]~out$time[out$year==2010], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2010], out.compare_NEE$NEE[out.compare_NEE$year==2010], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2010]~out$time[out$year==2010], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2010], out.compare_NDVI$NDVI[out.compare_NDVI$year==2010], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2011
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2011]~out$time[out$year==2011], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2011], out.compare_NEE$NEE[out.compare_NEE$year==2011], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2011]~out$time[out$year==2011], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011], out.compare_NDVI$NDVI[out.compare_NDVI$year==2011], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2012
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2012]~out$time[out$year==2012], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2012], out.compare_NEE$NEE[out.compare_NEE$year==2012], ylim=c(-4, 4), xlim=c(-4,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2012]~out$time[out$year==2012], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2012], out.compare_NDVI$NDVI[out.compare_NDVI$year==2012], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2013
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2013], out.compare_NEE$NEE[out.compare_NEE$year==2013], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013], out.compare_NDVI$NDVI[out.compare_NDVI$year==2013], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")


#PLOT DATA FOR 2014
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2014]~out$time[out$year==2014], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2014], out.compare_NEE$NEE[out.compare_NEE$year==2014], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2014]~out$time[out$year==2014], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2014], out.compare_NDVI$NDVI[out.compare_NDVI$year==2014], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")


#PLOT DATA FOR 2015
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2015]~out$time[out$year==2015], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2015], out.compare_NEE$NEE[out.compare_NEE$year==2015], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2015]~out$time[out$year==2015], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2015], out.compare_NDVI$NDVI[out.compare_NDVI$year==2015], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")





#linear regressions for all years
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]
par(mfrow=c(2,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE~data.compare_NEE$NEE)
plot(data.compare_NEE$NEE, out.compare_NEE$NEE, xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI~data.compare_NDVI$NDVI)
plot(data.compare_NDVI$NDVI, out.compare_NDVI$NDVI, xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI,col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")


summary(reg_NEE)
summary(reg_NDVI)


#linear regressions for 2009
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2009]~data.compare_NEE$NEE[data.compare_NEE$Year==2009])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2009], out.compare_NEE$NEE[out.compare_NEE$year==2009], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2009]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2009])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2009], out.compare_NDVI$NDVI[out.compare_NDVI$year==2009], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_NDVI)



#linear regressions for 2010
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2010]~data.compare_NEE$NEE[data.compare_NEE$Year==2010])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2010], out.compare_NEE$NEE[out.compare_NEE$year==2010], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2010]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2010])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2010], out.compare_NDVI$NDVI[out.compare_NDVI$year==2010], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_NDVI)



#linear regressions for 2011
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2011]~data.compare_NEE$NEE[data.compare_NEE$Year==2011])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2011], out.compare_NEE$NEE[out.compare_NEE$year==2011], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2011]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011], out.compare_NDVI$NDVI[out.compare_NDVI$year==2011], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_NDVI)



#linear regressions for 2012
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2012]~data.compare_NEE$NEE[data.compare_NEE$Year==2012])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2012], out.compare_NEE$NEE[out.compare_NEE$year==2012], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2012]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2012])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2012], out.compare_NDVI$NDVI[out.compare_NDVI$year==2012], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_NDVI)


#linear regressions for 2013
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2013]~data.compare_NEE$NEE[data.compare_NEE$Year==2013])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2013], out.compare_NEE$NEE[out.compare_NEE$year==2013], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2013]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013], out.compare_NDVI$NDVI[out.compare_NDVI$year==2013], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_NDVI)

#linear regressions for 2009-2012
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year!=2013]~data.compare_NEE$NEE[data.compare_NEE$Year!=2013])
plot(data.compare_NEE$NEE[data.compare_NEE$Year!=2013], out.compare_NEE$NEE[out.compare_NEE$year!=2013], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year!=2013]~data.compare_NDVI$NDVI[data.compare_NDVI$Year!=2013])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year!=2013], out.compare_NDVI$NDVI[out.compare_NDVI$year!=2013], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_NDVI)



####RESIDUAL ANALYSIS###
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]
resid.NEE = data.compare_NEE$NEE - out.compare_NEE$NEE
resid_NEE = data.frame(time=out.compare_NEE$time, Year=out.compare_NEE$year, DOY=out.compare_NEE$DOY, resid.NEE)
head(resid_NEE)


resid.NDVI = data.compare_NDVI$NDVI - out.compare_NDVI$NDVI
resid_NDVI = data.frame(time=out.compare_NDVI$time, Year=out.compare_NDVI$year, DOY=out.compare_NDVI$DOY, resid.NDVI)
head(resid_NDVI)

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(resid_NEE$DOY, resid_NEE$resid.NEE, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_NDVI$DOY, resid_NDVI$resid.NDVI, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")

rmse <- function(x){
  sqrt(mean(x^2))
}

rmse.NEE=rmse(resid.NEE)
rmse.NDVI=rmse(resid.NDVI)
rmse.NEE;rmse.NDVI

#mean residuals across years

resid.NEE.mean = tapply(resid_NEE$resid.NEE, resid_NEE$DOY, mean, na.rm=TRUE)
resid.NDVI.mean = tapply(resid_NDVI$resid.NDVI, resid_NDVI$DOY, mean, na.rm=TRUE)
time=seq(140,250,1)
resid.meansflux=data.frame(DOY=time, resid.GPP.mean, resid.NEE.mean, resid.Re.mean)
time=seq(149,250,1)
resid.meansNDVI=data.frame(DOY=time, resid.NDVI.mean)

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(resid.meansflux$DOY, resid.meansflux$resid.NEE.mean, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansNDVI$DOY, resid.meansNDVI$resid.NDVI.mean, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")



rmse.NEE=rmse(resid.NEE.mean)
rmse.NDVI=rmse(resid.NDVI.mean)
rmse.NEE;rmse.NDVI

#calculate RMSE by year:

tapply(resid_NEE$resid.NEE, resid_NEE$Year, rmse)
tapply(resid_NDVI$resid.NDVI, resid_NDVI$Year, rmse)



##look at relationship between spring NEE RMSE and DOY.minGDD
#first, filter data for spring only
head(resid_NEE)
rmse.NEE = tapply(resid_NEE$resid.NEE, resid_NEE$Year, rmse)
RMSE.spring=NULL
years = unique(data$year) #tells you which years we have data for 
senday=as.vector(tapply(data$senDOY, data$year, mean)) 
for (i in 1: length(years)){
  year.i = years[i] #select year
  data.year = subset(resid_NEE, resid_NEE$Year==year.i) #subset for that year
  resid_NEE.spring = data.year[data.year$DOY<=senday[i],] #subset for spring
  RMSE.spring[i] = rmse(resid_NEE.spring$resid.NEE) #calculate rmse
}
RMSE.spring #show yearly values
startday = as.vector(tapply(data$startDOY, data$year, mean)) 
endday = as.vector(tapply(data$endDOY, data$year, mean)) 
length.season = endday-startday
length.season
NEE_springRMSE = data.frame(RMSEspring = RMSE.spring, RMSEall = rmse.NEE, start.day = startday, end.day=endday, length=length.season)#create data frame
NEE_springRMSE #view

par(mfrow=c(3,1))
reg.spring = lm(NEE_springRMSE$RMSEall~NEE_springRMSE$start.day) #run linear model
plot(NEE_springRMSE$RMSEall~NEE_springRMSE$start.day) #plot data
abline(reg.spring, col="red") #add model line
summary(reg.spring) #view model stats

reg.fall = lm(NEE_springRMSE$RMSEall~NEE_springRMSE$end.day) #run linear model
plot(NEE_springRMSE$RMSEall~NEE_springRMSE$end.day) #plot data
abline(reg.fall, col="red") #add model line
summary(reg.fall) #view model stats

reg.length = lm(NEE_springRMSE$RMSEall~NEE_springRMSE$length) #run linear model
plot(NEE_springRMSE$RMSEall~NEE_springRMSE$length) #plot data
abline(reg.length, col="red") #add model line
summary(reg.length) #view model stats

#CHECK RELATIONSHIPS WITH FORCINGS
par(mfrow=c(2,1))
data.check = data[match(out.compare_NEE$time[out.compare_NEE$DOY>=160 & out.compare_NEE$DOY<=240], data$time),]
data.compareNEE = (data.compare_NEE[match(data.compare_NEE$Time, data.check$time),])
data.compareNEE = data.compareNEE[is.na(data.compareNEE$Time)==FALSE,]
out.compareNEE = (out.compare_NEE[match(out.compare_NEE$time, data.check$time),])
out.compareNEE = out.compareNEE[is.na(out.compareNEE$time)==FALSE,]
out.compare = (out[match(out$time, data.check$time),])


plot(data.compareNEE$NEE~data.check$PAR_ARF, pch=16, ylab="NEE", xlab="PAR")
points(out.compareNEE$NEE~data.check$PAR_ARF, col="red")
plot(data.compareNEE$NEE~data.check$Temp_ARF, pch=16, ylab="NEE", xlab="TEMP")
points(out.compareNEE$NEE~data.check$Temp_ARF, col="red")

plot(out.compare$GPP~out.compare$PAR, pch=16, ylab="GPP", xlab="PAR")
plot(out.compare$GPP~out.compare$Temp, pch=16, ylab="GPP", xlab="TEMP")

plot(out.compare$GPP~out.compare$LAI, pch=16, ylab="GPP", xlab="LAI")
plot(out.compare$LAI~out.compare$Temp, pch=16, ylab="LAI", xlab="TEMP")

plot(out$GPP~out$PAR, pch=16, ylab="GPP", xlab="PAR")
plot(out$GPP~out$Temp, pch=16, ylab="GPP", xlab="TEMP")

par(mfrow=c(3,1), mar=c(4,4,1,4))
plot(out.compare$Re~out.compare$Temp, ylab="Re", xlab="Temp", pch=16)
plot(out.compare$Rh~out.compare$Temp, col="red", ylab="Rh", xlab="Temp", pch=16)
plot(out.compare$Ra~out.compare$Temp, col="forestgreen", ylab="Ra", xlab="Temp", pch=16)


plot(out$Re~out$time, ylab="Respiration", xlab="Time", pch=16)
points(out$Rh~out$time, col="red")
points(out$Ra~out$time, col="forestgreen")
sum(out$Rh)/sum(out$Re)
sum(out$Ra)/sum(out$Re)

plot(out$Available_N~out$Temp, ylab="Available N", xlab="Temp")
par(mfrow=c(1,2), mar=c(4,4,1,2))
plot(out$Ntrans~out$Temp, ylab="Net N Min", xlab="Temp")
plot(out$Uptake~out$Temp, ylab="N Uptake", xlab="Temp")

par(mfrow=c(2,1), mar=c(4,4,1,4))
plot(out$DOY, out$TFN, xlab="DOY", ylab="TFN") # first plot
plot(out$DOY, out$LAI, xlab="DOY", ylab="LAI") # first plot
par(new = TRUE)
plot(out$DOY, out$NDVI, ylim=c(0, 1), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, col="red",col.axis="red",las=1)

plot(out$LAI~out$NDVI, ylab="LAI", xlab="NDVI")
plot(out$Available_N~out$time)
par(new = TRUE)
plot(out$time, out$NDVI, ylim=c(0, 1), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, col="red",col.axis="red",las=1)






