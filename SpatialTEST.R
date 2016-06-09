

dat = data.frame(read.csv("Summary_AllSites.csv")) #select data file
head(dat) #fiew first 6 rows


#put it all into a table
data.all = data.frame(matrix(1, 1, 12))
colnames(data.all) = c("Latitude", "Year", "DOY", "LST.avg", "PAR.avg", "NDVI.avg", "senDOY", 
                       "startDOY", "endDOY", "scal.seas", "scal.temp", "Tavg")
head(data.all)

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
  Year=rep(2000,length(DOY))
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


data.all = data.all[-1,]
head(data.all)
data.sorted = data.all[with(data.all, order(data.all$Tavg, data.all$Latitude, data.all$DOY)), ]
write.csv(data.sorted, "SpatialTest_060916") #save CSV 

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

for(i in 1:length(out$NDVI_MODIS)){
  if(out$NDVI_MODIS[i] < 0){
    out$NDVI_MODIS[i] = 0
  }
}

for(i in 1:length(data.sorted$NDVI.avg)){
  if(data.sorted$NDVI.avg[i] < 0){
    data.sorted$NDVI.avg[i] = 0
  }
}

par(mar=c(4,5,2,2), las=1)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), widths=c(2,2))

plot(out$NDVI_MODIS, pch=16, ylim=c(0,0.8), ylab="NDVI", xlab="Time")
points(data.sorted$NDVI.avg, col="red")
legend("topleft", cex=1.5, legend=c("CCaN NDVI", "MODIS NDVI"), bty="n", pch=c(16,1), col=c("black", "red"))


#pull out data for just growing season
out_GS = out[out$DOY>=140 & out$DOY <=250,]
data.sorted_GS = data.sorted[data.sorted$DOY>=140 & data.sorted$DOY <=250,]
reg_GS = lm(out_GS$NDVI_MODIS~data.sorted_GS$NDVI.avg)
summary(reg_GS)
plot(out_GS$NDVI_MODIS~data.sorted_GS$NDVI.avg, pch=16, ylim=c(0,0.8), xlim=c(0,0.8), ylab="CCaN", xlab="MODIS", main="All Growing Season NDVI")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", bty="n", cex=1.25, legend= bquote(italic(R)^2 == .(format(summary(reg_GS)$adj.r.squared, digits=2))))



#calculate GS avg NDVI
out_avg = tapply(out_GS$NDVI_MODIS, out_GS$Tavg, mean)
data_avg = tapply(data.sorted_GS$NDVI.avg, data.sorted_GS$Tavg, mean)
reg_avg = lm(out_avg~data_avg)
summary(reg_avg)
plot(out_avg~data_avg, pch=16, ylim=c(0.2,0.8), xlim=c(0.2,0.8), ylab="CCaN", xlab="MODIS", main="Growing Season Average NDVI")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", bty="n", cex=1.25, legend= bquote(italic(R)^2 == .(format(summary(reg_avg)$adj.r.squared, digits=2))))


#calculate temperature sensitivity
Temp_avg = unique(data.sorted$Tavg)


sens.CCaN = lm(out_avg~Temp_avg)
summary(sens.CCaN)

sens.MODIS = lm(data_avg~Temp_avg)
summary(sens.MODIS)


par(mfrow=c(2,1))
plot(data.sorted$LST.avg, pch=16, col="red", ylab="LST", xlab="Time")
plot(data.sorted$PAR.avg, pch=16, col="blue", ylab="PAR", xlab="Time")

head(data.sorted)

