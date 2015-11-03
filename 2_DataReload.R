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

#create a smoothed temperature scalar for GPP
#average of current sample, 7 future samples, and 7 past samples
filt=rep(1/15,15)
GDDslope.sm = filter(data$GDD.slope, filt, sides=2)
is.na(GDDslope.sm) #the last 20 samples are NA
GDDslope.sm[is.na(GDDslope.sm)]=0 #set these to zero
plot(data$GDD.slope, type="l")
lines(GDDslope.sm, col="red", lwd="3")
data=data.frame(data, GDDslope_sm=GDDslope.sm)

Tmaxsm.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  Tmaxsm.day[i]=max(data.year$GDDslope_sm)
}
Tmaxsm.day # max temps for each year
Tmaxsm.mean=mean(Tmaxsm.day)

scal.temp.sm=NULL
for (i in 1:length(GDDslope.sm)){
  scal.temp.sm[i] = (GDDslope.sm[i] - 0)/(Tmaxsm.mean-0) #growing degree day scalar
}
plot(scal.temp.sm, type="l")


#GDD slope is equal to positive temperatures, use that to calculate temperature scalar
#first need to determine max and for each year (min is just zero)
#Tmax.day = NA
#GDDmax.day = NA
#for (i in 1: length(years)){
#  year.i = years[i]
#  data.year = subset(data, data$year==year.i)
#  Tmax.day[i]=max(data.year$GDD.slope)
#  GDDmax.day[i]=max(data.year$GDD)
#}
#Tmax.day # max temps for each year
#GDDmax.day
#Tmax.mean=mean(Tmax.day)
#GDDmax.mean=mean(GDDmax.day)
#
#scal.temp=NULL
#for (i in 1:length(data$GDD.slope)){
#  scal.temp[i] = (data$GDD.slope[i] - 0)/(Tmax.mean-0) #growing degree day scalar
#}
#plot(scal.temp, type="l")

#scal.GDD=NULL
#for (i in 1:length(data$GDD)){
#  scal.GDD[i] = (data$GDD[i] - 0)/(GDDmax.mean-0) #growing degree day scalar
#  if(data$DOY[i]>data$DOY.sen[i]){
#   scal.GDD[i]=0
#  }
#}
#plot(scal.GDD, type="l")

#what if we add the GDD scalar and the smoothed temp scalar together?
#scal.new = scal.temp.sm+scal.GDD
#rescale to 1
#scal.add=NULL
#for (i in 1:length(scal.new)){
#  scal.add[i] = (scal.new[i] - min(scal.new))/(max(scal.new)-min(scal.new)) #growing degree day scalar
#}

#par(mfrow=c(3,1), mar=c(4,4,0.5,2))
#plot(scal.GDD, type="l")
#plot(scal.temp.sm, type="l")
#plot(scal.add, type="l")

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

par(mfrow=c(2,1))
plot(Tmax.diff1, type="l", xlim=c(1,365))
lines(Tmaxdiff.sm, col="red", lwd="3")
plot(scal.diff[1:365])


#create sigmoidal scalar to help model capture spring GPP
plot(data$GDD)
#need to figure out which DOY was the day when GDDs began to increase
years = unique(data$year) #tells you which years we have data for 
minGDD.day = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  minGDD.day[i] = min(data.year$DOY[which(data.year$GDD.slope>5)])
}
minGDD.day
par(mfrow=c(1,1))
plot(data$GDD~data$time, type="l", ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
abline(v=c(minGDD.day+c(0,365,365+365,365+365+366, 365+365+366+365)))

#day.sat = 175-minGDD.day #decided that spring will end at DOY 175
num.days = c(365, 365, 365, 366, 365)
minGDD = rep(c(minGDD.day), c(num.days))
#daySAT = rep(c(day.sat), c(num.days))
data = data.frame(data, DOY.minGDD = minGDD) #daySAT = daySAT)
head(data)

scal.spring=NULL
for (i in 1:length(data$DOY)){
  xsat = (175-data$DOY.minGDD[i])
  x =  data$DOY[i] - data$DOY.minGDD[i] #calculate number of days since start of growing season
  scal.spring[i] = (1*x)/(xsat+x)
  if(data$DOY[i]<data$DOY.minGDD[i]){
    scal.spring[i]=0
  }
}
plot(scal.spring)
points(scal.temp.sm, col="red")

#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_vis, method="linear", rule=2)
#scalGDD.d1 <- approxfun(x=data$time, y=scal.GDD, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalspring.d1 <- approxfun(x=data$time, y=scal.spring, method="linear", rule=2)
scaldiff.d1 <- approxfun(x=data$time, y=scal.diff, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
DOYsen.d1 <- approxfun(x=data$time, y=data$DOY.sen, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)
