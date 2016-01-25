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
PAR.filled = (4.401*PAR.filled)-6.515 #convert to mol m-2 s-1
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
#numyears = 100
#DOY.spin = rep(data$DOY, numyears)
#LST.spin = rep(data$LST.avg, numyears)
#PAR.spin = rep(data$PAR.avg, numyears)
#scal.temp.spin = rep(scal.temp, numyears)
#scal.seas.spin = rep(scal.seas, numyears)

#time = seq(1:length(DOY.spin))

#Step 4: make into functions so that it will be continuous in the model
#Temp.d1 <- approxfun(x=time, y=LST.spin, method="linear", rule=2)
#PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
#scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin, method="linear", rule=2)
#scalseason.d1 <- approxfun(x=time, y=scal.seas.spin, method="linear", rule=2)
#DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)

#OPEN 3_Model.R and run it the first time
#out= data.frame(solvemodel(params, state)) #creates table of model output

#head(out)
#plot(out$Biomass_C)
#out$Biomass_C[36235] #starting value of final year
#plot(out$Biomass_N)
#out$Biomass_N[36235] #starting value of final year
#plot(out$SOM_C)
#out$SOM_C[36235] #starting value of final year
#plot(out$SOM_N)
#out$SOM_N[36235] #starting value of final year
#plot(out$Available_N)
#out$Available_N[36235] #starting value of final year

#record starting values in spreadsheet
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
state <- c( Biomass_C = 684.5, 
            Biomass_N = 12.9, 
            SOM_C = 19358.7, 
            SOM_N = 854.1,
            Available_N = 1.6)

out= data.frame(solvemodel(params, state)) #creates table of model output










