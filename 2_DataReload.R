############re-loading the data

#set working directory to C-N-model
data = read.csv("InputData_Processed.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data)


#plot the data
par(mfrow=c(3,1), mar=c(4,4,0.5,2))
plot(data$estTemp_avg~data$time, type="l", ylab = "Daily Avg Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

#need to calculate DOY of senescence 
years = unique(data$year) #tells you which years we have data for 
sen.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  sen.day[i] = min(data.year$DOY[which(data.year$estTemp_min<=0 & data.year$DOY>180)])
}
sen.day
num.days = c(365, 365, 365, 366, 365)
senDOY = rep(c(sen.day), c(num.days))
data = data.frame(data, senDOY = senDOY)
head(data)

#create a temperature scalar
Tmax.day = NA
Tmin.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Tmax.day[i]=max(data.year$estTemp_avg)
  Tmin.day[i]=min(data.year$estTemp_avg)
}
Tmax.day # max temps for each year
Tmax.mean=mean(Tmax.day)
Tmin.mean=mean(Tmin.day)

scal.temp=NULL
for (i in 1:length(data$estTemp_avg)){
  scal.temp[i] = (data$estTemp_avg[i] - Tmin.mean)/(Tmax.mean-Tmin.mean) 
}

#rescale to 1
minscal = min(scal.temp)
maxscal = max(scal.temp)
for (i in 1:length(scal.temp)){
  scal.temp[i] = (scal.temp[i] - minscal)/(maxscal-minscal) 
}
plot(scal.temp, type="l")


#create a smoothed temperature scalar
#average of current sample, 10 future samples, and 10 past samples
filt=rep(1/21,21)
Temp.sm = filter(scal.temp, filt, sides=2)
is.na(Temp.sm) #shows that last few are NAs
Temp.sm[is.na(Temp.sm)]=0 #set these to zero
plot(scal.temp, type="l")
lines(Temp.sm, col="red", lwd="3")
data=data.frame(data, Temp_sm=Temp.sm)

Tmaxsm.day = NA
Tminsm.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
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

#rescale to 1
minscal = min(scal.temp.sm)
maxscal = max(scal.temp.sm)
for (i in 1:length(scal.temp.sm)){
  scal.temp.sm[i] = (scal.temp.sm[i] - minscal)/(maxscal-minscal)
}

plot(scal.temp.sm, type="l")


#create GPP scalar to assist with shoulder seasons
par(mfrow=c(1,1))
plot(data$Albedo)
#need to figure out which DOY was the day when snow began to melt
years = unique(data$year) #tells you which years we have data for 
melt.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  melt.day[i] = min(data.year$DOY[which(data.year$Albedo<0.2)])
}
melt.day
num.days = c(365, 365, 365, 366, 365)
meltDOY = rep(c(melt.day), c(num.days))
data = data.frame(data, meltDOY = meltDOY)
head(data)

#now determine day of first frost
years = unique(data$year) #tells you which years we have data for 
frost.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  frost.day[i] = min(data.year$DOY[which(data.year$estTemp_avg<=0 & data.year$DOY>180)])
}
frost.day
num.days = c(365, 365, 365, 366, 365)
frostDOY = rep(c(frost.day), c(num.days))
data = data.frame(data, frostDOY = frostDOY)
head(data)

#need to figure out which DOY was the day when snow fell
years = unique(data$year) #tells you which years we have data for 
snow.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  snow.day[i] = min(data.year$DOY[which(data.year$Albedo>0.2 & data.year$DOY>180)])
}
snow.day
num.days = c(365, 365, 365, 366, 365)
snowDOY = rep(c(snow.day), c(num.days))
data = data.frame(data, snowDOY = snowDOY)
head(data)


#now determine peak season day
#figure out which corresponds to peak in GPP
data.compare2=read.csv("Assimilation_data_ALL.csv")

years = unique(data$year) #tells you which years we have data for 
peakGPP.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data.compare2, data.compare2$Year==year.i)
  maxGPP.year = max(data.year$GPP,na.rm=TRUE)
  peakGPP.day[i] = data.year$DOY[which(data.year$GPP==maxGPP.year)]
}
peakGPP.day

peakTemp.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  maxTemp.year = max(data.year$Temp_ARF)
  peakTemp.day[i] = data.year$DOY[which(data.year$Temp_ARF==maxTemp.year)]
}
peakTemp.day

peakPAR.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  maxPAR.year = max(data.year$PAR_ARF)
  peakPAR.day[i] = data.year$DOY[which(data.year$PAR_ARF==maxPAR.year)]
}
peakPAR.day

mid.day=round((frost.day+melt.day)/2)
mid.day

#figure out which is best
mean(abs(peakTemp.day-peakGPP.day))
mean(abs(peakPAR.day-peakGPP.day))
mean(abs(mid.day-peakGPP.day)) 


plot(data.compare2$GPP~data.compare2$Time, col="forestgreen", xlim=c(1,1826))
abline(v=c(snow.day+c(0,365,365+365,365+365+366, 365+365+366+365)))
num.days = c(365, 365, 365, 366, 365)
peakDOY = rep(mid.day, c(num.days))
data = data.frame(data, peakDOY = peakDOY)
head(data)

#now determine slope of GPP increase after melt
plot(data.compare2$GPP~data.compare2$Time, col="forestgreen", xlim=c(1,1826))
head(data.compare2) #view data
data.compare2 = data.compare2[!is.na(data.compare2$GPP),]
head(data.compare2) #view data

#determine data to calculate slope
years = unique(data$year) #tells you which years we have data for 
startGPP.day = NA #day which GPP starts
startGPP = NA #GPP at starting day
endGPP.day = NA #day which GPP ends
endGPP = NA #GPP at end day
peakGPP = NA #GPP at peak day
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data.compare2, data.compare2$Year==year.i)
  startGPP.day[i] = min(data.year$DOY)
  startGPP[i] = data.year$GPP[which(data.year$DOY==startGPP.day[i])]
  endGPP.day[i] = max(data.year$DOY)
  endGPP[i] = data.year$GPP[which(data.year$DOY==endGPP.day[i])]
  peakGPP[i] = data.year$GPP[which(data.year$DOY==peakGPP.day[i])]
}
startGPP.day
startGPP
endGPP.day
endGPP
peakGPP.day
peakGPP
slope.GPP.i = (peakGPP-startGPP)/(peakGPP.day-startGPP.day) #calculate initial slope for each year
slope.GPP.f = (peakGPP-endGPP)/abs((peakGPP.day-endGPP.day)) #calculate final slope for each year
slope.GPP.i 
slope.GPP.f
mean(slope.GPP.i)
mean(slope.GPP.f)

#see how slope relates to other variables
Tmax = tapply(data$estTemp_avg, data$year, max)
Pmax = tapply(data$PAR_ARF, data$year, max)
plot(slope.GPP, Tmax) #no relationship
plot(slope.GPP, Pmax) #no relationship
slope.GPP


#now, create a scalar
scal.GPP.i = NA
for(i in 1:length(data$time)){
  if(data$DOY[i]<=data$meltDOY[i]){
    scal.GPP.i[i] = 0 
  }
  if(data$DOY[i]>data$meltDOY[i]){
    scal.GPP.i[i] = 0 + mean(slope.GPP.i)*(data$DOY[i]-data$meltDOY[i])
  }
  if(scal.GPP.i[i]>1){
    scal.GPP.i[i] = 1
  }
}

plot(scal.GPP.i)

scal.GPP.f = NA
for(i in 1:length(data$time)){  
  if(data$DOY[i]<data$frostDOY[i]){
    scal.GPP.f[i] = 0 + mean(slope.GPP.f)*(data$frostDOY[i]-data$DOY[i])
  }
  if(data$DOY[i]>=data$frostDOY[i]){
    scal.GPP.f[i] = 0
  }
  
  if(scal.GPP.f[i]>1){
    scal.GPP.f[i] = 1
  }
}

plot(scal.GPP.f)

scal.GPP=scal.GPP.i*scal.GPP.f
plot(scal.GPP)

###############seasonal scalar##############

peakPAR_DOY = rep(peakPAR.day, c(365, 365, 365, 366, 365))
peakTemp_DOY = rep(peakTemp.day, c(365, 365, 365, 366, 365))

scal.seas=rep(1, length(data$DOY))
for (i in 1:length(data$DOY)){
  if(data$DOY[i]<data$meltDOY[i]){ #prior to snow melt
    scal.seas[i]=0
  }
  if(data$DOY[i]>=data$meltDOY[i]){ #after melt
    if(data$DOY[i]<=data$senDOY[i]){ #prior to peak
      slope = 1/(data$senDOY[i]-data$meltDOY[i])
      scal.seas[i] = 0+(slope*(data$DOY[i]-data$meltDOY[i]))
    }
    if(data$DOY[i]>data$senDOY[i] & data$DOY[i]<data$snowDOY[i]){ #after peak but before frost
      slope = 1/(data$snowDOY[i]-data$senDOY[i])
      scal.seas[i] = 0+(slope*(data$snowDOY[i]-data$DOY[i]))
    }
    if(data$DOY[i]>=data$snowDOY[i]){ #after frost
      scal.seas[i]=0
    }
  }
}

plot(scal.seas)


#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
albedo.d1 <- approxfun(x=data$time, y=data$Albedo, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
DOYpeak.d1 <- approxfun(x=data$time, y=data$peakDOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)
