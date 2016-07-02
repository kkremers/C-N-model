

dat = data.frame(read.csv("Summary_AllSites.csv")) #select data file
head(dat) #fiew first 6 rows


#put it all into a table
data.all = NULL

#########
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
  Year=rep(2000+i,length(DOY))
  Latitude = rep(lat.i, length(DOY))
  data=data.frame(Latitude, Year,DOY,LST.avg,PAR.avg,NDVI.avg)
  
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
  for (n in 1:length(data$DOY)){
    if(data$DOY[n]<data$startDOY[n]){ #prior to snow melt
      scal.seas[n]=0
    }
    if(data$DOY[n]>=data$startDOY[n]){ #after melt
      if(data$DOY[n]<=data$senDOY[n]){ #prior to peak
        slope = 1/(data$senDOY[n]-data$startDOY[n])
        scal.seas[n] = 0+(slope*(data$DOY[n]-data$startDOY[n]))
      }
      if(data$DOY[n]>data$senDOY[n] & data$DOY[n]<data$endDOY[n]){ #after peak but before frost
        slope = 1/(data$endDOY[n]-data$senDOY[n])
        scal.seas[n] = 0+(slope*(data$endDOY[n]-data$DOY[n]))
      }
      if(data$DOY[n]>=data$endDOY[n]){ #after frost
        scal.seas[n]=0
      }
    }
  }
  
  data = data.frame(data, scal.seas = scal.seas)
  
  #temperature scalar
  Tmax = max(data$LST.avg)
  Tmin = min(data$LST.avg)
  scal.temp=NULL
  for (n in 1:length(data$LST.avg)){
    scal.temp[n] = (data$LST.avg[n] - Tmin)/(Tmax-Tmin) 
  }
  
  data = data.frame(data, scal.temp = scal.temp)
  
  #calculate growing season average temp
  Temp_GS = data$LST.avg[data$DOY>=data$startDOY[1] & data$DOY <= data$endDOY[1]]
  Temp_avg = mean(Temp_GS)
  Tavg = rep(Temp_avg, num.days)
  
  data = data.frame(data, Tavg=Tavg)
  
  
  data.all = rbind(data.all, data) #bind new row to table
}

#############

head(data.all)
data.sorted = data.all[with(data.all, order(data.all$Tavg, data.all$Latitude, data.all$DOY)), ]
write.csv(data.sorted, "SpatialTest_061116") #save CSV 

plot(data.sorted$Tavg)
plot(data.sorted$LST.avg)
plot(data.sorted$PAR.avg)
plot(data.sorted$NDVI.avg)

head(data.sorted)
time = seq(1:length(data.sorted$DOY))

#Step 4: make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=data.sorted$LST.avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=data.sorted$PAR.avg, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=data.sorted$scal.temp, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=data.sorted$scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=data.sorted$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=time, y=data.sorted$Year, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=time, y=data.sorted$Tavg, method="linear", rule=2)

param.best
state.best

out=data.frame(solvemodel(param.best, state.best))
head(out)

for(i in 1:length(data.sorted$NDVI.avg)){
  if(data.sorted$NDVI.avg[i] < 0){
    data.sorted$NDVI.avg[i] = 0
  }
}

par(mar=c(4,5,2,2), las=1)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2,2))

plot(out$NDVI, pch=16, ylim=c(0,0.8), ylab="NDVI", xlab="Time")
points(data.sorted$NDVI.avg, col="red")
legend("topleft", cex=1.5, legend=c("CCaN NDVI", "MODIS NDVI"), bty="n", pch=c(16,1), col=c("black", "red"))


#pull out data for just growing season
out_GS = out[out$DOY>=140 & out$DOY <=250,]
data.sorted_GS = data.sorted[data.sorted$DOY>=140 & data.sorted$DOY <=250,]
reg_GS = lm(out_GS$NDVI~data.sorted_GS$NDVI.avg)
summary(reg_GS)
plot(out_GS$NDVI~data.sorted_GS$NDVI.avg, pch=16, ylim=c(0,0.8), xlim=c(0,0.8), ylab="CCaN", xlab="MODIS", main="All Growing Season NDVI")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", bty="n", cex=1.25, legend= bquote(italic(R)^2 == .(format(summary(reg_GS)$adj.r.squared, digits=2))))



#calculate GS avg NDVI
out_avg = tapply(out_GS$NDVI, out_GS$Tavg, mean)
data_avg = tapply(data.sorted_GS$NDVI.avg, data.sorted_GS$Tavg, mean)
reg_avg = lm(out_avg~data_avg)
summary(reg_avg)
plot(out_avg~data_avg, pch=16, ylim=c(0.2,0.8), xlim=c(0.2,0.8), ylab="CCaN", xlab="MODIS", main="Growing Season Average NDVI")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", bty="n", cex=1.25, legend= bquote(italic(R)^2 == .(format(summary(reg_avg)$adj.r.squared, digits=2))))


#calculate temperature sensitivity
Temp_avg=NULL
Temps = unique(data.sorted_GS$Tavg)
for(i in 1:length(Temps)){
  Temps.i = Temps[i]
  dat.i = subset(data.sorted_GS, data.sorted_GS$Tavg == Temps.i)
  
  Temp.avg = mean(dat.i$LST.avg)
  Temp_avg = c(Temp_avg, Temp.avg)
}

out.nospin_avg = data.frame(Tavg = Temp_avg, NDVI = as.numeric(out_avg))
head(out.nospin_avg)
write.csv(out.nospin_avg, "SpatialTest_061116_NDVI_NOSPIN") #save CSV 

sens.CCaN = lm(out_avg~Temp_avg)
summary(sens.CCaN)

sens.MODIS = lm(data_avg~Temp_avg)
summary(sens.MODIS)


par(mfrow=c(2,1))
plot(data.sorted$LST.avg, pch=16, col="red", ylab="LST", xlab="Time")
plot(data.sorted$PAR.avg, pch=16, col="blue", ylab="PAR", xlab="Time")

head(data.sorted)


#now re-run using model spin-up for each site and compare

time = seq(1:length(data.sorted$DOY))

#Step 4: make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=data.sorted$LST.avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=data.sorted$PAR.avg, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=data.sorted$scal.temp, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=data.sorted$scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=data.sorted$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=time, y=data.sorted$Year, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=time, y=data.sorted$Tavg, method="linear", rule=2)

param.best
state.best

#load in summary table created from Manuscript1_plots.R
spatial.summ = read.csv("CaTT_Summary_061116")
head(spatial.summ)
head(data.sorted)
#need to re-sort spatial.summ in the same order as data.sorted
spatial.summ = spatial.summ[with(spatial.summ, order(spatial.summ$Tavg, spatial.summ$Latitude)), ]
head(spatial.summ)

NDVI.out = NULL
DOY.out = NULL
Tavg.out = NULL

#now need to run model for each year using the different starting values
latitudes = unique(dat$Latitude)

for(i in 1:length(latitudes)){
  lat.i = latitudes[i]  
  dat.i = subset(data.sorted, Latitude==lat.i)
  state.i = subset(spatial.summ, Latitude==lat.i)
  state = as.numeric((state.i[3:7]))
  names(state) = names(state.best)
  
  
  time = seq(1:length(dat.i$DOY))
  
  #Step 4: make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=dat.i$LST.avg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=dat.i$PAR.avg, method="linear", rule=2)
  scaltemp.d1 <- approxfun(x=time, y=dat.i$scal.temp, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=dat.i$scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=dat.i$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=dat.i$Year, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=time, y=dat.i$Tavg, method="linear", rule=2)

  out=data.frame(solvemodel(param.best, state))

  
  NDVI.out = c(NDVI.out, out$NDVI)
  DOY.out = c(DOY.out, out$DOY)
  Tavg.out = c(Tavg.out, out$Tavg)
  
}
 
length(data.sorted[,1])
length(NDVI.out) 
length(DOY.out)
length(Tavg.out)

spinup.run = data.frame(Tavg = Tavg.out, DOY = DOY.out, NDVI = NDVI.out)
head(spinup.run) #need to sort this by Tavg and DOY

spinup.sorted = spinup.run[with(spinup.run, order(spinup.run$Tavg, spinup.run$DOY)), ]
head(spinup.sorted)
data.sorted.all = cbind(data.sorted, NDVI_CCaN = spinup.sorted$NDVI)
head(data.sorted.all)
write.csv(data.sorted.all, "SpatialTest_061116") #save CSV 

par(mar=c(4,5,2,2), las=1)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2,2))

plot(data.sorted.all$NDVI_CCaN, pch=16, ylim=c(0,0.8), ylab="NDVI", xlab="Time")
points(data.sorted$NDVI.avg, col="red")
legend("topleft", cex=1.5, legend=c("CCaN NDVI", "MODIS NDVI"), bty="n", pch=c(16,1), col=c("black", "red"))


#pull out data for just growing season
data.sorted_GS = data.sorted.all[data.sorted.all$DOY>=140 & data.sorted.all$DOY <=250,]
reg_GS = lm(data.sorted_GS$NDVI_CCaN~data.sorted_GS$NDVI.avg)
summary(reg_GS)
plot(data.sorted_GS$NDVI_CCaN~data.sorted_GS$NDVI.avg, pch=16, ylim=c(0,0.8), xlim=c(0,0.8), ylab="CCaN", xlab="MODIS", main="All Growing Season NDVI")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", bty="n", cex=1.25, legend= bquote(italic(R)^2 == .(format(summary(reg_GS)$adj.r.squared, digits=2))))



#calculate GS avg NDVI
out_avg = tapply(data.sorted_GS$NDVI_CCaN, data.sorted_GS$Tavg, mean)
data_avg = tapply(data.sorted_GS$NDVI.avg, data.sorted_GS$Tavg, mean)
reg_avg = lm(out_avg~data_avg)
summary(reg_avg)
plot(out_avg~data_avg, pch=16, ylim=c(0.2,0.8), xlim=c(0.2,0.8), ylab="CCaN", xlab="MODIS", main="Growing Season Average NDVI")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", bty="n", cex=1.25, legend= bquote(italic(R)^2 == .(format(summary(reg_avg)$adj.r.squared, digits=2))))

Temp_avg


sens.CCaN = lm(out_avg~Temp_avg)
summary(sens.CCaN)

sens.MODIS = lm(data_avg~Temp_avg)
summary(sens.MODIS)

data.sorted_GS
head(data.sorted.all)
