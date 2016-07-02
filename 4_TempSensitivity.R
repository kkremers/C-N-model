
######check temperature sensitivity###########
#first run 2_DataReload.R
#then run 3_Model.R


#Create data for temperature sensitivity test
Temp.sens= tapply(data$Temp_ARF, data$DOY, mean)
Temp.sens.all = c(Temp.sens, Temp.sens+0.25, Temp.sens+0.5, Temp.sens+0.75, Temp.sens+1,
                  Temp.sens+1.25, Temp.sens+1.5, Temp.sens+1.75, Temp.sens+2)
plot(Temp.sens.all)

PAR.sens= tapply(data$PAR_ARF, data$DOY, mean)
plot(PAR.sens)
PAR.sens.all = rep(PAR.sens, 9)
plot(PAR.sens.all)
DOY.sens = rep(seq(1:366), 9)
Years.sens = rep(c(1,2,3,4,5,6,7,8,9), c(366,366,366,366,366,366,366,366,366))
dat.sens = data.frame(Year = Years.sens, DOY = DOY.sens, Temp=Temp.sens.all, PAR=PAR.sens.all)
head(dat.sens)

#need to calculate DOY of senescence 
years = unique(dat.sens$Year) #tells you which years we have data for 
sen.day = NA
num.days = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  sen.day[i] = min(data.year$DOY[which(data.year$Temp<=10 & data.year$DOY>200)])
  num.days[i] = length(data.year[,1])
}
sen.day
num.days
senDOY = rep(c(sen.day), c(num.days))
dat.sens = data.frame(dat.sens, senDOY = senDOY)
head(dat.sens)

#create a temperature scalar
Tmax.day = NA
Tmin.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  Tmax.day[i]=max(data.year$Temp)
  Tmin.day[i]=min(data.year$Temp)
}
Tmax.day # max temps for each year
Tmax.mean=mean(Tmax.day)
Tmin.mean=mean(Tmin.day)

scal.temp=NULL
for (i in 1:length(dat.sens$Temp)){
  scal.temp[i] = (dat.sens$Temp[i] - Tmin.mean)/(Tmax.mean-Tmin.mean) 
}

#rescale to 1
minscal = min(scal.temp)
maxscal = max(scal.temp)
for (i in 1:length(scal.temp)){
  scal.temp[i] = (scal.temp[i] - minscal)/(maxscal-minscal) 
}
plot(scal.temp, type="l")


#create a smoothed temperature scalar
#average of current sample, 5 future samples, and 5 past samples
filt=rep(1/11,11)
Temp.sm = filter(dat.sens$Temp, filt, sides=2, circular=TRUE)
plot(dat.sens$Temp, type="l")
lines(Temp.sm, col="red", lwd="3")
dat.sens=data.frame(dat.sens, Temp_sm=Temp.sm)

Tmaxsm.day = NA
Tminsm.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  Tmaxsm.day[i]=max(data.year$Temp_sm)
  Tminsm.day[i]=min(data.year$Temp_sm)
}
Tmaxsm.day # max temps for each year
Tmaxsm.mean=mean(Tmaxsm.day)
Tminsm.mean=mean(Tminsm.day)

scal.temp.sm=NULL
for (i in 1:length(Temp.sm)){
  scal.temp.sm[i] = (Temp.sm[i] - Tminsm.mean)/(Tmaxsm.mean-Tminsm.mean) 
}

plot(scal.temp.sm, type="l")

#need to calculate slope for each 7 day period
start = seq(1, length(Temp.sm), 8) #create a sequence of start iterations for slope calculations
end = seq(8, length(Temp.sm), 8) #create a sequence of end iterations for slope calculations
start
end

#need to look at slope to determine end of season
slope=rep(NA, length(Temp.sm))
for(i in 1:length(end)){
  slope.i = (Temp.sm[end[i]]-Temp.sm[start[i]])/7
  start.i = start[i]
  end.i = end[i]
  slope[start.i:end.i]=rep(slope.i, 8)
}
slope
slope[3289:length(slope)]=slope[3288] #these points don't really matter, but I don't want them to be NAs
slope
dat.sens=data.frame(dat.sens, slope=slope)
head(dat.sens)


spring.test = rep(0, length(slope))
for(i in 9:length(dat.sens$slope)-8){
  if(slope[i]>0 & slope[i+8]>0){
    spring.test[i]=1
  }
}
spring.test

fall.test = rep(0, length(slope))
for(i in 9:length(dat.sens$slope)-8){
  if(slope[i]<0 & slope[i+8]<0){
    fall.test[i]=1
  }
}
fall.test


par(mfrow=c(2,1))
plot(Temp.sm)
lines(spring.test*10, col="red")
plot(Temp.sm)
lines(fall.test*10, col="red")



dat.sens=data.frame(dat.sens, springtest=spring.test, falltest=fall.test)
head(dat.sens)

years = unique(dat.sens$Year) #tells you which years we have data for 
start.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  start.day[i] = min(data.year$DOY[which(data.year$DOY>120 & data.year$Temp>=-5 & data.year$springtest==1)])
}
start.day
startDOY = rep(c(start.day), c(num.days))
dat.sens = data.frame(dat.sens, startDOY = startDOY)
head(dat.sens)

end.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  end.day[i] = min(data.year$DOY[which(data.year$DOY>240 & data.year$Temp<=0 & data.year$falltest==1)])
}
end.day
endDOY = rep(c(end.day), c(num.days))
dat.sens = data.frame(dat.sens, endDOY = endDOY)
head(dat.sens)

###############seasonal scalar##############

scal.seas=rep(1, length(dat.sens$DOY))
for (i in 1:length(dat.sens$DOY)){
  if(dat.sens$DOY[i]<dat.sens$startDOY[i]){ #prior to snow melt
    scal.seas[i]=0
  }
  if(dat.sens$DOY[i]>=dat.sens$startDOY[i]){ #after melt
    if(dat.sens$DOY[i]<=dat.sens$senDOY[i]){ #prior to peak
      slope = 1/(dat.sens$senDOY[i]-dat.sens$startDOY[i])
      scal.seas[i] = 0+(slope*(dat.sens$DOY[i]-dat.sens$startDOY[i]))
    }
    if(dat.sens$DOY[i]>dat.sens$senDOY[i] & dat.sens$DOY[i]<dat.sens$endDOY[i]){ #after peak but before frost
      slope = 1/(dat.sens$endDOY[i]-dat.sens$senDOY[i])
      scal.seas[i] = 0+(slope*(dat.sens$endDOY[i]-dat.sens$DOY[i]))
    }
    if(dat.sens$DOY[i]>=dat.sens$endDOY[i]){ #after frost
      scal.seas[i]=0
    }
  }
}

plot(scal.seas)

#FINALLY, need to calculate average growing season temperature
#pull out GS data (DOY > 150 & DOY < 250)
head(dat.sens)

Temp_avg = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  Temp_GS = data.year$Temp[data.year$DOY>=data.year$startDOY[1] & data.year$DOY <= data.year$endDOY[1]]
  Temp_avg[i] = mean(Temp_GS)
}
Temp_avg
Temp_avg = rep(c(Temp_avg), c(num.days))
dat.sens = data.frame(dat.sens, Temp_avg = Temp_avg)
head(dat.sens)
time=seq(1:length(dat.sens[,1]))
dat.sens=data.frame(time=time, dat.sens)
head(dat.sens)

time = seq(1:length(dat.sens$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=dat.sens$time, y=dat.sens$Temp, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=dat.sens$time, y=dat.sens$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=dat.sens$time, y=dat.sens$PAR, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=dat.sens$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=dat.sens$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=dat.sens$time, y=dat.sens$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=dat.sens$time, y=dat.sens$year, method="linear", rule=2)


####NOW NEED TO RUN SPINUP

Temp.spin= tapply(dat.sens$Temp, dat.sens$DOY, mean)
plot(Temp.spin)
Temp_GS = dat.sens$Temp[dat.sens$DOY>=dat.sens$startDOY[1] & dat.sens$DOY <= data$endDOY[1]]
AvgTempGS = mean(Temp_GS)
TempAvg.spin = rep(AvgTempGS, 366)
PAR.spin = tapply(dat.sens$PAR, dat.sens$DOY, mean)
plot(PAR.spin)
DOY.spin=seq(1:366)
Year.spin=rep(1,366)
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
numyears = 30
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


state = state.best

#OPEN 3_Model.R and run it the first time
out.spin= data.frame(solvemodel(param.best, state)) #creates table of model output
plot(out.spin$Biomass_N)
plot(out.spin$Biomass_C)
end.time = length(out.spin[,1])
#adjust starting values
state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
            Biomass_N =  out.spin$Biomass_N[end.time], 
            SOM_C = out.spin$SOM_C[end.time], 
            SOM_N = out.spin$SOM_N[end.time],
            Available_N = out.spin$Available_N[end.time])


time = seq(1:length(dat.sens$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=dat.sens$time, y=dat.sens$Temp, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=dat.sens$time, y=dat.sens$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=dat.sens$time, y=dat.sens$PAR, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=dat.sens$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=dat.sens$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=dat.sens$time, y=dat.sens$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=dat.sens$time, y=dat.sens$Year, method="linear", rule=2)

out= data.frame(solvemodel(param.best, state)) #creates table of model output



#plot fluxes and NDVI
plot(out$NEE)
plot(out$GPP)
plot(out$Ra)
plot(out$Rh)
plot(out$NDVI)



#plot pools
par(mfrow=c(3,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="forestgreen", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="forestgreen",  main = "Biomass N", xlab="", ylab="g N m-2")
plot(out$SOM_C~out$time, type="l", col="brown", main = "SOM C", xlab="", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="brown", main = "SOM N", xlab="Time (days)", ylab="g N m-2")
plot(out$Available_N~out$time, type="l", col="blue", main = "Available_ N", xlab="Time (days)", ylab="g N m-2",lty=2)


#now calculate NDVI temperature sensivity
#need to calcualte gs average NDVI and temp for each year
head(out)
tail(out)
head(dat.sens)
Tavg = unique(dat.sens$Temp_avg)
Tavg

NDVIavg = NA
years=unique(out$year)
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(dat.sens, dat.sens$Year==year.i)
  out.year = subset(out, out$year==year.i)
  NDVI_GS = out.year$NDVI[out.year$DOY>=data.year$startDOY[1] & out.year$DOY <= data.year$endDOY[1]]
  NDVIavg[i] = mean(NDVI_GS)
}
NDVIavg

sens.mod = lm(NDVIavg~Tavg)
summary(sens.mod)



