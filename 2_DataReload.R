############re-loading the data

#set working directory to C-N-model
data = read.csv("InputData_Processed.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data)


#plot the data
par(mfrow=c(2,2), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$GDD~data$time, ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")


plot(data$GDD[1:365]~data$DOY[1:365], ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")


GDD.slope = rep(0, length = length(data$GDD))
for (i in 2: length(data$GDD)){
  GDD.slope[i] = data$GDD[i] - data$GDD[i-1]
}

plot(GDD.slope) #this is really just temperature but for only positive values
data = cbind(data, GDD.slope=GDD.slope)
head(data)
plot(data$GDD.slope) #there are 4 points that need to be set to 0 because this is when we switch years
data$GDD.slope[data$time[which(data$GDD.slope == min(data$GDD.slope))]] = 0
data$GDD.slope[data$time[which(data$GDD.slope == min(data$GDD.slope))]] = 0
data$GDD.slope[data$time[which(data$GDD.slope == min(data$GDD.slope))]] = 0
data$GDD.slope[data$time[which(data$GDD.slope == min(data$GDD.slope))]] = 0
data$time[which(data$GDD.slope == min(data$GDD.slope))]
par(mfrow=c(1,1), mar=c(4,4,0.5,2))
plot(data$DOY[1:365], data$GDD.slope[1:365], type="l")

#need to figure out which DOY was the day when GDDs level off
years = unique(data$year) #tells you which years we have data for 
delGDDmax.day = NA
DOY.sen.year = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  delGDDmax = data.year$DOY[which(data.year$DOY>182 & data.year$GDD.slope<1 & data.year$GDD.slope>0)]
  DOY.sen.year[i]=delGDDmax[1]
}
DOY.sen.year #looked at this data to determine cutoff point
num.days = c(365, 365, 365, 366, 365)
DOY.sen = rep(c(DOY.sen.year), c(num.days))
data = data.frame(data, DOY.sen = DOY.sen)
head(data)
par(mfrow=c(1,1))
plot(data$GDD~data$time, type="l", ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
abline(v=c(DOY.sen.year+c(0,365,365+365,365+365+366, 365+365+366+365)))



#create a temperature scalar
Tmax.day = NA
Tmin.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Tmax.day[i]=max(data.year$Temp_ARF)
  Tmin.day[i]=min(data.year$Temp_ARF)
}
Tmax.day # max temps for each year
Tmax.mean=mean(Tmax.day)
Tmin.mean=mean(Tmin.day)

scal.temp=NULL
for (i in 1:length(data$Temp_ARF)){
  scal.temp[i] = (data$Temp_ARF[i] - Tmin.mean)/(Tmax.mean-Tmin.mean) 
}

#rescale to 1
minscal = min(scal.temp)
maxscal = max(scal.temp)
for (i in 1:length(scal.temp)){
  scal.temp[i] = (scal.temp[i] - minscal)/(maxscal-minscal) 
}
plot(scal.temp, type="l")


#create a smoothed temperature scalar
#average of current sample, 7 future samples, and 7 past samples
filt=rep(1/21,21)
Temp.sm = filter(data$Temp_ARF, filt, sides=2)
is.na(Temp.sm) #the last 20 samples are NA
Temp.sm[is.na(Temp.sm)]=0 #set these to zero
plot(data$Temp_ARF, type="l")
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

#create a smoothed PAR scalar
#average of current sample, 7 future samples, and 7 past samples
filt=rep(1/21,21)
PAR.sm = filter(data$PAR_ARF, filt, sides=2)
is.na(PAR.sm) #the last 20 samples are NA
PAR.sm[is.na(PAR.sm)]=0 #set these to zero
plot(data$PAR_ARF, type="l")
lines(PAR.sm, col="red", lwd="3")
data=data.frame(data, PAR_sm=PAR.sm)

Pmaxsm.day = NA
Pminsm.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Pmaxsm.day[i]=max(data.year$PAR_sm)
  Pminsm.day[i]=min(data.year$PAR_sm)
}
Pmaxsm.day # max temps for each year
Pmaxsm.mean=mean(Pmaxsm.day)
Pminsm.mean=mean(Pminsm.day)

scal.PAR.sm=NULL
for (i in 1:length(PAR.sm)){
  scal.PAR.sm[i] = (PAR.sm[i] - Pminsm.mean)/(Pmaxsm.mean-Pminsm.mean)
}

#rescale to 1
minscal = min(scal.PAR.sm)
maxscal = max(scal.PAR.sm)
for (i in 1:length(scal.PAR.sm)){
  scal.PAR.sm[i] = (scal.PAR.sm[i] - minscal)/(maxscal-minscal) 
}

plot(scal.PAR.sm, type="l")



#making a new scalar for NDVI calculation

Tmaxmean.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Tmaxmean.day[i]=max(data.year$Temp_ARF)
}
Tmaxmean.day # max temps for each year
Tmax.avg = mean(Tmaxmean.day) #calculate average

Tmax.diff = data$Temp_ARF-Tmax.avg
plot(Tmax.diff[1:365])
filt=rep(1/30,30)
Tmaxdiff.sm = filter(Tmax.diff, filt, sides=2)
is.na(Tmaxdiff.sm) #the last 7 samples are NA
Tmaxdiff.sm[is.na(Tmaxdiff.sm)]=0 #set these to zero
plot(Tmax.diff, type="l", xlim=c(1,365))
lines(Tmaxdiff.sm, col="red", lwd="3")
Tmax.diff1 = Tmax.diff
Tmax.diff=Tmaxdiff.sm
data=data.frame(data, TmaxDiff = Tmax.diff)
head(data)
Tmaxmean.diff = NA
Tminmean.diff = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Tmaxmean.diff[i]=max(data.year$TmaxDiff, na.rm=TRUE)
  Tminmean.diff[i]=min(data.year$TmaxDiff, na.rm=TRUE)
}
Tmaxmean.diff # max temps for each year
Tmaxdiff.avg = mean(Tmaxmean.diff) #calculate average
Tminmean.diff # min temps for each year
Tmindiff.avg = mean(Tminmean.diff) #calculate average


scal.diff=NULL
for (i in 1:length(Tmax.diff)){
  scal.diff[i] = (Tmax.diff[i] - Tmindiff.avg)/(Tmaxdiff.avg-Tmindiff.avg)
}

#rescale to 1
minscal = min(scal.diff)
maxscal = max(scal.diff)
for (i in 1:length(scal.diff)){
  scal.diff[i] = (scal.diff[i] - minscal)/(maxscal-minscal)
}

par(mfrow=c(2,1))
plot(Tmax.diff1, type="l", xlim=c(1,365))
lines(Tmaxdiff.sm, col="red", lwd="3")
plot(scal.diff)


#create sigmoidal scalar to help model capture spring GPP
par(mfrow=c(2,1))
plot(data$GDD)
plot(data$Albedo)
#need to figure out which DOY was the day when snow began to melt
years = unique(data$year) #tells you which years we have data for 
melt.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  melt.day[i] = min(data.year$DOY[which(data.year$Albedo<0.15)])
}
melt.day
par(mfrow=c(1,1))
plot(data$GDD~data$time, type="l", ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
abline(v=c(melt.day+c(0,365,365+365,365+365+366, 365+365+366+365)))
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
  frost.day[i] = min(data.year$DOY[which(data.year$Temp_ARF<=0 & data.year$DOY>180)])
}
frost.day
par(mfrow=c(1,1))
plot(data$GDD~data$time, type="l", ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
abline(v=c(frost.day+c(0,365,365+365,365+365+366, 365+365+366+365)))
num.days = c(365, 365, 365, 366, 365)
frostDOY = rep(c(frost.day), c(num.days))
data = data.frame(data, frostDOY = frostDOY)
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

avg.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  avg.day[i] = round((peakTemp.day[i]+peakPAR.day[i])/2)
}
avg.day

#figure out which is best
sum(abs(peakTemp.day-peakGPP.day))
sum(abs(peakPAR.day-peakGPP.day))
sum(abs(avg.day-peakGPP.day)) 
sum(abs(mid.day-peakGPP.day)) #this is the best


plot(data.compare2$GPP~data.compare2$Time, col="forestgreen", xlim=c(1,1826))
abline(v=c(avg.day+c(0,365,365+365,365+365+366, 365+365+366+365)))
num.days = c(365, 365, 365, 366, 365)
peakDOY = rep(mid.day, c(num.days))
data = data.frame(data, peakDOY = peakDOY)
head(data)

scal.GPP=NULL
for (i in 1:length(data$DOY)){
  if(data$DOY[i]<data$meltDOY[i]){ #prior to snow melt
    scal.GPP[i]=0
  }
  if(data$DOY[i]>=data$meltDOY[i]){ #after melt
    if(data$DOY[i]<=data$peakDOY[i]){ #prior to peak
      xsat = (data$peakDOY[i]-data$meltDOY[i])/2
      x=data$DOY[i]-data$meltDOY[i] #calculate number of days since snowmelt
      scal.GPP[i]=(1*x)/(xsat+x)
      #slope = 1/(data$peakDOY[i]-data$meltDOY[i])
      #scal.GPP[i] = 0+(slope*(data$DOY[i]-data$meltDOY[i]))
    }
    if(data$DOY[i]>data$peakDOY[i] & data$DOY[i]<data$frostDOY[i]){ #after peak but before frost
      slope = 1/(data$frostDOY[i]-data$peakDOY[i])
      scal.GPP[i] = 0+(slope*(data$frostDOY[i]-data$DOY[i]))
    }
    if(data$DOY[i]>=data$frostDOY[i]){ #after frost
      scal.GPP[i]=0
    }
  }
}

plot(scal.GPP)


#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
albedo.d1 <- approxfun(x=data$time, y=data$Albedo, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalGPP.d1 <- approxfun(x=data$time, y=scal.GPP, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
DOYsen.d1 <- approxfun(x=data$time, y=data$DOY.sen, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)
