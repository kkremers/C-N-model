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
abline(v=c(DOY.sen.year+c(0,365,365+365,365+365+366,365+365+366+365)))

#last, we need to create a smoothed temperature scalar for LAI
#average of current sample, 7 future samples, and 7 past samples
filt=rep(1/15,15)
GDDslope.sm = filter(data$GDD.slope, filt, sides=2)
is.na(GDDslope.sm) #the last 20 samples are NA
GDDslope.sm[is.na(GDDslope.sm)]=0 #set these to zero
plot(data$GDD.slope, type="l")
lines(GDDslope.sm, col="red", lwd="3")

scal.temp.sm=NULL
for (i in 1:length(GDDslope.sm)){
  scal.temp.sm[i] = (GDDslope.sm[i] - min(GDDslope.sm))/(max(GDDslope.sm)-min(GDDslope.sm)) #growing degree day scalar
}
plot(scal.temp.sm, type="l")

scal.temp=NULL
for (i in 1:length(data$GDD.slope)){
  scal.temp[i] = (data$GDD.slope[i] - min(data$GDD.slope))/(max(data$GDD.slope)-min(data$GDD.slope)) #growing degree day scalar
}
plot(scal.temp, type="l")


scal.GDD=NULL
for (i in 1:length(data$GDD)){
  scal.GDD[i] = (data$GDD[i] - min(data$GDD))/(max(data$GDD)-min(data$GDD)) #growing degree day scalar
  if(data$DOY[i]>data$DOY.sen[i]){
   scal.GDD[i]=0
  }
}
plot(scal.GDD, type="l")

#what if we add the GDD scalar and the smoothed temp scalar together?
scal.new = scal.temp+scal.GDD

#rescale to 1
scal.add=NULL
for (i in 1:length(scal.new)){
  scal.add[i] = (scal.new[i] - min(scal.new))/(max(scal.new)-min(scal.new)) #growing degree day scalar
}

par(mfrow=c(1,1), mar=c(4,4,0.5,2))
plot(scal.add, type="l")
plot(scal.temp, type="l")

#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_vis, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp, method="linear", rule=2)
scaltempsm.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalGDD.d1 <- approxfun(x=data$time, y=scal.GDD, method="linear", rule=2)
scaladd.d1 <- approxfun(x=data$time, y=scal.add, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
DOYsen.d1 <- approxfun(x=data$time, y=data$DOY.sen, method="linear", rule=2)
