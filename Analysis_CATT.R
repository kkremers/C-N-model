#This analysis is to see how CCaN performs along a latitudinal gradient
#Using data from Sal's CATT network 

#Step 1: read in data and linearly interpolate

dat = data.frame(read.csv(file.choose())) #select data file
plot(dat$LST_avg) #check for outliers

head(dat) #fiew first 6 rows
time=seq(1:length(dat[,1])) #generate a sequence of x values for interpolation

LST.filled = approx(time, dat$LST_avg, time, method = "linear", rule = 2)$y #fill LST
LST.filled = LST.filled-273.15 #convert to celcius
plot(LST.filled)

PAR.filled = approx(time, dat$PAR, time, method = "linear", rule = 2)$y #fill PAR
PAR.filled = 3.5947*PAR.filled #convert to mol m-2 s-1
plot(PAR.filled)

NDVI.filled = approx(time, dat$NDVI, time, method = "linear", rule = 2)$y #fill NDVI
plot(NDVI.filled)

#bind these columns to dat
dat=cbind(dat, LST.filled, PAR.filled, NDVI.filled)
head(dat)

#Step 2: calculate decadal averages
LST.avg = tapply(dat$LST.filled, dat$DOY, mean)
plot(LST.avg)
PAR.avg = tapply(dat$PAR.filled, dat$DOY, mean)
plot(PAR.avg)
NDVI.avg = tapply(dat$NDVI.filled, dat$DOY, mean)
plot(NDVI.avg)
DOY=seq(1:366)
data=data.frame(DOY,LST.avg,PAR.avg,NDVI.avg)
head(data)


#Step 3: get ready for model input
#calculate scalars

#seasonality scalar
#DOY of senescence 
sen.day=min(data$DOY[which(data$LST.avg<=10 & data$DOY>200)])
sen.day
num.days = 366
senDOY = rep(sen.day, num.days)
data = data.frame(data, senDOY = senDOY)
head(data)

#start day
start.day=min(data$DOY[which(data$LST.avg>=-5 & data$DOY>120)])
start.day
startDOY = rep(start.day, num.days)
data = data.frame(data, startDOY = startDOY)
head(data)

#end day
end.day=min(data$DOY[which(data$LST.avg<=0 & data$DOY>240)])
end.day
endDOY = rep(end.day, num.days)
data = data.frame(data, endDOY = endDOY)
head(data)


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

plot(scal.seas)



#temperature scalar
Tmax = max(data$LST.avg)
Tmin = min(data$LST.avg)
scal.temp=NULL
for (i in 1:length(data$LST.avg)){
  scal.temp[i] = (data$LST.avg[i] - Tmin)/(Tmax-Tmin) 
}

plot(scal.temp, type="l")

###################MODEL SPINUP######################
numyears = 100
DOY.spin = rep(data$DOY, numyears)
LST.spin = rep(data$LST.avg, numyears)
PAR.spin = rep(data$PAR.avg, numyears)
scal.temp.spin = rep(scal.temp, numyears)
scal.seas.spin = rep(scal.seas, numyears)

time = seq(1:length(DOY.spin))

#Step 4: make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=LST.spin, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=scal.seas.spin, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)

#OPEN 3_Model.R and run it the first time
state  <- c(Biomass_C = 722.51, 
            Biomass_N = 10.01, 
            SOM_C = 18389.02, 
            SOM_N = 762.70,
            Available_N = 1.1)
out.spin= data.frame(solvemodel(params, state)) #creates table of model output

#######################################################

#Run model for each site and record NDVI

time = seq(1:length(data$DOY))

#Step 4: make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=data$LST.avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=data$PAR.avg, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=scal.temp, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=data$DOY, method="linear", rule=2)


#OPEN 3_Model.R and run once to store function
#adjust starting values
state <- c( Biomass_C = out.spin$Biomass_C[36235], 
            Biomass_N = out.spin$Biomass_N[36235], 
            SOM_C = out.spin$SOM_C[36235], 
            SOM_N = out.spin$SOM_N[36235],
            Available_N = out.spin$Available_N[36235])

out= data.frame(solvemodel(params, state)) #creates table of model output

#determine max NDVI & record in spreadsheet
CCaN_max = max(out$NDVI)
MODIS_max = max(data$NDVI)

#determine GS average NDVI 
#pull out GS data (DOY > 150 & DOY < 250)
CCaN_gsNDVI = out$NDVI[out$DOY>=150 & out$DOY <=250]
MODIS_gsNDVI = data$NDVI.avg[data$DOY>=150 & data$DOY <=250]
#calculate average
CCaN_avg = mean(CCaN_gsNDVI)
MODIS_avg = mean(MODIS_gsNDVI)

#determine GS Avg Temp & PAR 
#pull out GS data (DOY > 150 & DOY < 250)
Temp_GS = data$LST.avg[data$DOY>=150 & data$DOY <=250]
PAR_GS = data$PAR.avg[data$DOY>=150 & data$DOY <=250]
#calculate average
Tavg = mean(Temp_GS)
PARavg = mean(PAR_GS)

#put it all into a table
#######only run these the first time#########
#summary = data.frame(matrix(1, 0, 13))
#colnames(summary) = c("Latitude", "Biomass_C", "Biomass_N", "SOM_C", "SOM_N", "Available_N", "CCaN_max", "MODIS_max", "CCaN_avg", "MODIS_avg", "Tmax", "Tavg", "PARavg")
#head(summary)
###########################################

summary = rbind(summary, c(dat[1,1], state, CCaN_max, MODIS_max, CCaN_avg, MODIS_avg, Tmax, Tavg, PARavg)) #bind new row to table
summary #view table

write.csv(summary, "CaTT_Summary") #save CSV 


###########PLOTS#####################

#Convert to LAI and TFN to compare
MODIS_LAI = 0.0026*exp(8.0783*summary$MODIS_max)
CCaN_LAI = 0.0026*exp(8.0783*summary$CCaN_max)
summary=cbind(summary, CCaN_LAI = CCaN_LAI, MODIS_LAI = MODIS_LAI)

MODIS_TFN = (MODIS_LAI*1.29)+0.31
CCaN_TFN = (CCaN_LAI*1.29)+0.31
summary=cbind(summary, MODIS_TFN=MODIS_TFN, CCaN_TFN=CCaN_TFN)
head(summary)


#regressions with temperature - NDVI
reg_MODIS.temp = lm(summary$MODIS_avg~summary$Tavg)
reg_CCaN.temp = lm(summary$CCaN_avg~summary$Tavg)

par(mfrow=c(1,3))
plot(summary$CCaN_avg~summary$Tavg, pch=16, ylim=c(0.3, 0.8), xlab="GS Avg Temp", ylab="GS Avg NDVI")
abline(reg_CCaN.temp)
points(summary$MODIS_avg~summary$Tavg, pch=16, col="forestgreen")
abline(reg_MODIS.temp, col="forestgreen")

#regressions with temperature - LAI
reg_MODISLAI.temp = lm(summary$MODIS_LAI~summary$Tavg)
reg_CCaNLAI.temp = lm(summary$CCaN_LAI~summary$Tavg)

plot(summary$CCaN_LAI~summary$Tavg, pch=16, ylim=c(0, 1.1), xlab="GS Avg Temp", ylab="GS Max LAI")
abline(reg_CCaNLAI.temp)
points(summary$MODIS_LAI~summary$Tavg, pch=16, col="forestgreen")
abline(reg_MODISLAI.temp, col="forestgreen")


#regressions with temperature - TFN
reg_MODISTFN.temp = lm(summary$MODIS_TFN~summary$Tavg)
reg_CCaNTFN.temp = lm(summary$CCaN_TFN~summary$Tavg)

plot(summary$CCaN_TFN~summary$Tavg, pch=16, ylim=c(0, 2), xlab="GS Avg Temp", ylab="GS Max TFN")
abline(reg_CCaNTFN.temp)
points(summary$MODIS_TFN~summary$Tavg, pch=16, col="forestgreen")
abline(reg_MODISTFN.temp, col="forestgreen")


#regression stats
summary(reg_MODIS.temp)
summary(reg_CCaN.temp)
summary(reg_MODISLAI.temp)
summary(reg_CCaNLAI.temp)
summary(reg_MODISTFN.temp)
summary(reg_CCaNTFN.temp)


#regressions with latitude - NDVI
reg_MODIS.lat = lm(summary$MODIS_avg~summary$Latitude)
reg_CCaN.lat = lm(summary$CCaN_avg~summary$Latitude)

par(mfrow=c(1,3))
plot(summary$CCaN_avg~summary$Latitude, pch=16, ylim=c(0.3, 0.8), xlab="Latitude", ylab="GS Avg NDVI")
abline(reg_CCaN.lat)
points(summary$MODIS_avg~summary$Latitude, pch=16, col="forestgreen")
abline(reg_MODIS.lat, col="forestgreen")

#regressions with temperature - LAI
reg_MODISLAI.lat = lm(summary$MODIS_LAI~summary$Latitude)
reg_CCaNLAI.lat = lm(summary$CCaN_LAI~summary$Latitude)

plot(summary$CCaN_LAI~summary$Latitude, pch=16, ylim=c(0, 1.1), xlab="Latitude", ylab="GS Max LAI")
abline(reg_CCaNLAI.lat)
points(summary$MODIS_LAI~summary$Latitude, pch=16, col="forestgreen")
abline(reg_MODISLAI.lat, col="forestgreen")


#regressions with temperature - TFN
reg_MODISTFN.lat = lm(summary$MODIS_TFN~summary$Latitude)
reg_CCaNTFN.lat = lm(summary$CCaN_TFN~summary$Latitude)

plot(summary$CCaN_TFN~summary$Latitude, pch=16, ylim=c(0, 2), xlab="Latitude", ylab="GS Max TFN")
abline(reg_CCaNTFN.lat)
points(summary$MODIS_TFN~summary$Latitude, pch=16, col="forestgreen")
abline(reg_MODISTFN.lat, col="forestgreen")


#regression stats
summary(reg_MODIS.lat)
summary(reg_CCaN.lat)
summary(reg_MODISLAI.lat)
summary(reg_CCaNLAI.lat)
summary(reg_MODISTFN.lat)
summary(reg_CCaNTFN.lat)



#Now make the same plots with the residuals

#calculate residuals
resid_NDVImax = abs(summary$CCaN_max-summary$MODIS_max)
resid_NDVIavg = abs(summary$CCaN_avg-summary$MODIS_avg)
resid_LAImax = abs(summary$CCaN_LAI-summary$MODIS_LAI)
resid_TFNmax = abs(summary$CCaN_TFN-summary$MODIS_TFN)

#regressions with temperature - NDVI
reg_temp.NDVIresid = lm(resid_NDVIavg~summary$Tavg)

par(mfrow=c(1,2))
plot(resid_NDVIavg~summary$Tavg, pch=16, col="blue", ylim=c(0, 0.2), xlab="GS Avg Temp", ylab="GS Avg NDVI Residuals (CCaN-MODIS)")
abline(reg_temp.NDVIresid, col="blue")


#regressions with latitude - NDVI
reg_lat.NDVIresid = lm(resid_NDVIavg~summary$Latitude)

plot(resid_NDVIavg~summary$Latitude, pch=16, col="blue", ylim=c(0, 0.2), xlab="Latitude", ylab="GS Avg NDVI Residuals (CCaN-MODIS)")
abline(reg_lat.NDVIresid, col="blue")

#regression stats
summary(reg_temp.NDVIresid)
summary(reg_lat.NDVIresid)


#make sensitivty bar graph
Sample = c("CCaN", "MODIS", "Goetz", "LTER")
Sensitivty = c(0.011, 0.031, 0.02, 0.022)
par(mfrow=c(1,1))
barplot(Sensitivty, names.arg=Sample, ylab="Temp Sensitivity")



#Plot modelled vs. measured
reg_NDVI = lm(summary$CCaN_avg~summary$MODIS_avg)
reg_LAI = lm(summary$CCaN_LAI~summary$MODIS_LAI)
par(mfrow=c(1,2))
plot(summary$CCaN_avg~summary$MODIS_avg, ylim=c(0.4,0.7), xlim=c(0.4,0.7), pch=16, ylab="CCaN GS Avg NDVI", xlab="MODIS GS avg NDVI")
abline(0,1, col="red")
abline(reg_NDVI)
plot(summary$CCaN_LAI~summary$MODIS_LAI, ylim=c(0.2,1.1), xlim=c(0.2,1.1), pch=16, ylab="CCaN GS Max LAI", xlab="MODIS GS Max LAI")
abline(0,1, col="red")
abline(reg_LAI)

summary(reg_NDVI)
summary(reg_LAI)
