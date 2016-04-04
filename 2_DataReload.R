############re-loading the data

#set working directory to C-N-model
data = read.csv("InputData_Processed.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data)


#plot the data
par(mfrow=c(2,1), mar=c(3,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Avg Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

#need to calculate DOY of senescence 
years = unique(data$year) #tells you which years we have data for 
sen.day = NA
num.days = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  sen.day[i] = min(data.year$DOY[which(data.year$Temp_ARF<=10 & data.year$DOY>200)])
  num.days[i] = length(data.year[,1])
}
sen.day
num.days
senDOY = rep(c(sen.day), c(num.days))
data = data.frame(data, senDOY = senDOY)
head(data)

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
#average of current sample, 5 future samples, and 5 past samples
filt=rep(1/11,11)
Temp.sm = filter(data$Temp_ARF, filt, sides=2, circular=TRUE)
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
slope[2921:length(slope)]=slope[2920] #these points don't really matter, but I don't want them to be NAs
slope
data=data.frame(data, slope=slope)
head(data)


spring.test = rep(0, length(slope))
for(i in 9:length(data$slope)-8){
  if(slope[i]>0 & slope[i+8]>0){
    spring.test[i]=1
  }
}
spring.test

fall.test = rep(0, length(slope))
for(i in 9:length(data$slope)-8){
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



data=data.frame(data, springtest=spring.test, falltest=fall.test)
head(data)

years = unique(data$year) #tells you which years we have data for 
start.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  start.day[i] = min(data.year$DOY[which(data.year$DOY>120 & data.year$Temp_ARF>=-5 & data.year$springtest==1)])
}
start.day
startDOY = rep(c(start.day), c(num.days))
data = data.frame(data, startDOY = startDOY)
head(data)

end.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  end.day[i] = min(data.year$DOY[which(data.year$DOY>240 & data.year$Temp_ARF<=0 & data.year$falltest==1)])
}
end.day
endDOY = rep(c(end.day), c(num.days))
data = data.frame(data, endDOY = endDOY)
head(data)

data.compare2=read.csv("Assimilation_data_ALL.csv")
days.tot = c(0, cumsum(num.days)[1:7])

par(mfrow=c(1,1))
plot(data.compare2$NEE~data.compare2$Time, col="forestgreen", pch=16, cex=0.5)
abline(v=c(start.day+days.tot), col="red")
abline(v=c(sen.day+days.tot))
abline(v=c(end.day+days.tot), col="blue")

###############seasonal scalar##############

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

plot(scal.seas)

#FINALLY, need to calculate average growing season temperature
#pull out GS data (DOY > 150 & DOY < 250)
head(data)

years = unique(data$year) #tells you which years we have data for 
Temp_avg = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Temp_GS = data.year$Temp_ARF[data.year$DOY>=data.year$startDOY[1] & data.year$DOY <= data.year$endDOY[1]]
  Temp_avg[i] = mean(Temp_GS)
}
Temp_avg
Temp_avg = rep(c(Temp_avg), c(num.days))
data = data.frame(data, Temp_avg = Temp_avg)
head(data)


time = seq(1:length(data$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)
