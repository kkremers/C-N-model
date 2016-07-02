###############MODEL UNCERTAINTY###################
load("Step2_061116.Rdata")
head(param.keep)
dat = data.frame(read.csv("Summary_AllSites.csv")) #select data file
head(dat) #fiew first 6 rows

#put it all into a table
summary = data.frame(matrix(1, 1, 3))
colnames(summary) = c("Latitude", "RunNumber", "CCaN_avg")
head(summary)

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
  data=data.frame(Year,DOY,LST.avg,PAR.avg,NDVI.avg)
  
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
  
  
  #temperature scalar
  Tmax = max(data$LST.avg)
  Tmin = min(data$LST.avg)
  scal.temp=NULL
  for (i in 1:length(data$LST.avg)){
    scal.temp[i] = (data$LST.avg[i] - Tmin)/(Tmax-Tmin) 
  }
  
  
  #calculate growing season average temp
  Temp_GS = data$LST.avg[data$DOY>=data$startDOY[1] & data$DOY <= data$endDOY[1]]
  Temp_avg = mean(Temp_GS)
  
  #Run spinup
  numyears = 30
  DOY.spin = rep(data$DOY, numyears)
  Year.spin = rep(data$Year, numyears)
  LST.spin = rep(data$LST.avg, numyears)
  PAR.spin = rep(data$PAR.avg, numyears)
  scal.temp.spin = rep(scal.temp, numyears)
  scal.seas.spin = rep(scal.seas, numyears)
  TempAvg.spin = rep(Temp_avg, length(DOY.spin))
  
  
  for(p in 1:1000){
  params = param.keep[p,]
  names(params) = names(param.keep)
  state = state.best
 
  #run spinup
  time = seq(1:length(DOY.spin))
  
  #Step 4: make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=LST.spin, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
  scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=scal.seas.spin, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
  
  
  out.spin= data.frame(solvemodel(params, state)) #creates table of model output
  
  
  #adjust starting values
  end.time = length(out.spin[,1])
  #adjust starting values
  state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
              Biomass_N = out.spin$Biomass_N[end.time], 
              SOM_C = out.spin$SOM_C[end.time], 
              SOM_N = out.spin$SOM_N[end.time],
              Available_N = out.spin$Available_N[end.time])
  
  
  #Run model record NDVI
  
  time = seq(1:length(data$DOY))
  
  #Step 4: make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=data$LST.avg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=data$PAR.avg, method="linear", rule=2)
  scaltemp.d1 <- approxfun(x=time, y=scal.temp, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=data$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=data$Year, method="linear", rule=2)
  
  
  out= data.frame(solvemodel(params, state)) #creates table of model output
  
  #determine GS average NDVI 
  #pull out GS data (DOY > 140 & DOY < 250)
  CCaN_gsNDVI = out$NDVI[data$DOY==161 | data$DOY==177 | data$DOY==193 | data$DOY==209 | data$DOY==225 | data$DOY==241]
  #calculate average
  CCaN_avg = mean(CCaN_gsNDVI)
  
  summary = rbind(summary, c(lat.i, p, CCaN_avg)) #bind new row to table
  } #end of parameter loop
}#end of latitude loop

head(summary)
summary=summary[-1,]
head(summary)
tail(summary)
write.csv(summary, "CaTT_uncertainty_062316") #save CSV 
#now, for each latitude, need to calculate mean and 90% CI
NDVI.mean = tapply(summary$CCaN_avg, summary$Latitude, mean)
NDVI.sd = tapply(summary$CCaN_avg, summary$Latitude, sd)
NDVI.95 = tapply(summary$CCaN_avg, summary$Latitude, quantile, 0.95)
NDVI.05 = tapply(summary$CCaN_avg, summary$Latitude, quantile, 0.05)
NDVI.ci = (NDVI.sd/sqrt(1000))*1.65

NDVI_uncertainty = data.frame(Latitude = names(NDVI.mean), mean=NDVI.mean, CI = NDVI.ci, p95 = NDVI.95, p05=NDVI.05)
head(NDVI_uncertainty)
write.csv(NDVI_uncertainty, "CaTT_uncertainty_summary")

