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




GDD.slope = rep(0, length = length(data$GDD))
for (i in 2: length(data$GDD)){
  GDD.slope[i] = data$GDD[i] - data$GDD[i-1]
}

plot(GDD.slope, ylim=c(0, 30)) #this is really just temperature but for only positive values
data = cbind(data, TempPos=GDD.slope)
head(data)

plot(data$TempPos)
data$time[which(data$TempPos == min(data$TempPos))]
data$TempPos[731] = 0
data$time[which(data$TempPos == min(data$TempPos))]
data$TempPos[1462] = 0
data$time[which(data$TempPos == min(data$TempPos))]
data$TempPos[366] = 0
data$time[which(data$TempPos == min(data$TempPos))]
data$TempPos[1096] = 0
plot(data$TempPos)

#need to figure out which DOY was the day when GDDs level off
years = unique(data$year) #tells you which years we have data for 
delGDDmax.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  delGDDmax = data.year$DOY[which(data.year$TempPos < 1)]
  delGDDmax.day = c(delGDDmax.day, delGDDmax)
}
delGDDmax.day #looked at this data to determine cutoff point
DOY.sen = rep(c(261, 264, 252, 269, 257), c(365, 365, 365, 366, 365))
data = data.frame(data, DOY.sen = DOY.sen)
head(data)
par(mfrow=c(1,1))
data$DOY
plot(data$GDD~data$time, type="l", ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
abline(v=261)
abline(v=264+365)
abline(v=252+365+365)
abline(v=269+365+365+365)
abline(v=257+365+365+365+366)


#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_vis, method="linear", rule=2)
TempPos.d1 <-approxfun(x=data$time, y=data$TempPos, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
DOYsen.d1 <- approxfun(x=data$time, y=data$DOY.sen, method="linear", rule=2)
