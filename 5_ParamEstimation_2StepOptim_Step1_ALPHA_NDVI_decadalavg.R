###LOAD DATA###
dat = data.frame(read.csv("Summary_AllSites.csv")) #select data file
head(dat) #fiew first 6 rows


#put it all into a table
data.ALL = NULL

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
    
  #bind these columns to dat
  dat.i=cbind(dat.i, LST.filled, PAR.filled)
  
  #Step 2: calculate decadal averages
  LST.avg = tapply(dat.i$LST.filled, dat.i$DOY, mean)
  PAR.avg = tapply(dat.i$PAR.filled, dat.i$DOY, mean)
  NDVI.avg = tapply(dat.i$NDVI, dat.i$DOY, mean, na.rm=TRUE)
  DOY=seq(1:366)
  Year=rep(2000+i,length(DOY))
  Latitude = rep(lat.i, length(DOY))
  data=data.frame(Latitude, Year, DOY, Year_DOY=interaction(Year, DOY, sep="_"), Temp = LST.avg,PAR = PAR.avg,NDVI = NDVI.avg)
  
  #seasonality scalar
  #DOY of senescence 
  sen.day=min(data$DOY[which(data$Temp<=10 & data$DOY>200)])
  sen.day
  num.days = 366
  senDOY = rep(sen.day, num.days)
  data = data.frame(data, senDOY = senDOY)
  
  #start day
  start.day=min(data$DOY[which(data$Temp>=-5 & data$DOY>120)])
  start.day
  startDOY = rep(start.day, num.days)
  data = data.frame(data, startDOY = startDOY)
  
  #end day
  end.day=min(data$DOY[which(data$Temp<=0 & data$DOY>240)])
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
  
  
  #calculate growing season average temp
  Temp_GS = data$Temp[data$DOY>=data$startDOY[1] & data$DOY <= data$endDOY[1]]
  Temp_avg = mean(Temp_GS)
  Tavg = rep(Temp_avg, num.days)
  
  data = data.frame(data, Tavg=Tavg)
  
  
  data.ALL = rbind(data.ALL, data) #bind new row to table
}


head(data.ALL)
tail(data.ALL)

data.assim = data.ALL[,c(1:4,7)]
head(data.assim)

sigma = rep(0.07, length(data.assim[,1]))
data.sigma = cbind(data.ALL[,c(1:4)], NDVI=sigma)
head(data.sigma)

state.dat = read.csv("CaTT_Summary_080416")

data.compare1=NULL
sigma.obs1 = NULL

for(i in 1:length(latitudes)){
  lat.i = latitudes[i]  
  dat.i = subset(data.ALL, Latitude==lat.i)
    time=seq(1:length(dat.i[,1]))

  #make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=dat.i$Temp, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=time, y=dat.i$Tavg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=dat.i$PAR, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=dat.i$scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=dat.i$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=dat.i$Year, method="linear", rule=2)
  
  state.i = subset(state.dat, Latitude==lat.i)
  state.i = as.numeric(state.i[3:7])
  names(state.i)=names(state) 
    
  out= data.frame(solvemodel(params, state.i)) #creates table of model output

  head(out)
  out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
  head(out1)
  data1=data.frame(cbind(time=time, NDVI=data.assim[data.assim$Latitude==lat.i,5]))
  sigma1 = data.frame(cbind(time=time, NDVI=data.sigma[data.sigma$Latitude==lat.i,5]))
  lat = rep(lat.i, length(data1[,1]))
  data1 = cbind(Latitude = lat, data1)
  sigma1 = cbind(Latitude = lat, sigma1)
  
  data.compare1 = rbind(data.compare1, data1)
  sigma.obs1 = rbind(sigma.obs1, sigma1)
}

head(data.compare1)
tail(data.compare1)
head(sigma.obs1)
tail(sigma.obs1)

####DATA EXPLORATION###
#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(0.046)
param.min=c(0.0014)

##STEP 1: Explore with BOTH NEE and NDVI

#other necessary knowns
n.param = length(params)#number of parameters to estimate
M = 100000 #number of iterations
D = 1 #number of data types being assimilated 
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+2])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 

#storage matrices
J = rep(1E100, M) #storage vector for cost function output
j = matrix(1E100, M, D) #to store error calculations for this iteration
all.draws = data.frame(matrix(1, M, n.param)) #storage for all parameter estimate iterations;
colnames(all.draws) = c(names(params))
param.est = data.frame(matrix(1, M, n.param)) #storage for accepted parameter estimate iterations;
param.est[1,]=params #change first row to current guess
all.draws[1,]=params #change first row to current guess
colnames(param.est) = c(names(params))
head(param.est) #check to make sure this is correct
head(all.draws)

#replace 1st row with values for current parameters
out.compare1=NULL
for(i in 1:length(latitudes)){
  lat.i = latitudes[i]  
  dat.i = subset(data.ALL, Latitude==lat.i)
  data.compare.i = subset(data.compare1, Latitude==lat.i)
  time=seq(1:length(dat.i[,1]))
  
  #make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=dat.i$Temp, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=time, y=dat.i$Tavg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=dat.i$PAR, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=dat.i$scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=dat.i$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=dat.i$Year, method="linear", rule=2)
  
  state.i = subset(state.dat, Latitude==lat.i)
  state.i = as.numeric(state.i[3:7])
  names(state.i)=names(state) 
  
  out= data.frame(solvemodel(params, state.i)) #creates table of model output
  out1 = out[match(data.compare.i$time, out$time),c(1,11)] #these columns need to match the ones that were pulled out before
  lat = rep(lat.i, length(out1[,1]))
  out1 = cbind(Latitude = lat, out1)

  out.compare1 = rbind(out.compare1, out1)
}  
rownames(out.compare1)=rownames(data.compare1)
head(out.compare1)
tail(out.compare1)
head(data.compare1)
tail(data.compare1)
head(sigma.obs1)
tail(sigma.obs1)

error.time=matrix(0, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
for (d in 1:D) { #for each data type
  for (m in 1:length(data.compare1$time)){ #for each timestep
    if(!is.na(data.compare1[m,d+2])){ #if there is data at that timestep for that data stream
      error.time[m,d]=((data.compare1[m,d+2] - out.compare1[m,d+2])/sigma.obs1[m,d+2])^2 #calculates the error at that timestep for that data stream
    } #end of if statement
    #if there was no data at that timestep, the error will remain "0" so that it will not impact the sum calculation in the next step
  } #end of time step loop
  
  j[1,d] = sum(error.time[,d]) #calculate cost function for each data stream
  
} #end of data type loop

J[1] = prod(j[1,]) #calculate aggregate cost function
head(J)
head(j)
head(param.est)
tail(J)
tail(j)
tail(param.est)


#set initial values
#anneal.temp0=1000000 #starting temperature
#anneal.temp=1000000 #starting temperature
reject=0 #reset reject counter
t=0.5


for (i in 2:M) {
  repeat{
    for(p in 1:n.param){ #for each parameter
      param.est[i,p] = param.est[i-1,p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
      all.draws[i,p] = param.est[i,p]
      parms = as.numeric(param.est[i,]) #parameters for model run
      names(parms) = names(params) #fix names
    } #end of parameter loop
    if(all(param.est[i,]>=param.min) & all(param.est[i,]<=param.max)){
        break
      } #end of if loop
    } #end of repeat
  
    
    out.compare1=NULL
  for(k in 1:length(latitudes)){
    lat.i = latitudes[k]  
    dat.i = subset(data.ALL, Latitude==lat.i)
    data.compare.i = subset(data.compare1, Latitude==lat.i)
    time=seq(1:length(dat.i[,1]))
    
    #make into functions so that it will be continuous in the model
    Temp.d1 <- approxfun(x=time, y=dat.i$Temp, method="linear", rule=2)
    TempAvg.d1 <- approxfun(x=time, y=dat.i$Tavg, method="linear", rule=2)
    PAR.d1 <- approxfun(x=time, y=dat.i$PAR, method="linear", rule=2)
    scalseason.d1 <- approxfun(x=time, y=dat.i$scal.seas, method="linear", rule=2)
    DOY.d1 <- approxfun(x=time, y=dat.i$DOY, method="linear", rule=2)
    Year.d1 <- approxfun(x=time, y=dat.i$Year, method="linear", rule=2)
    
    state.i = subset(state.dat, Latitude==lat.i)
    state.i = as.numeric(state.i[3:7])
    names(state.i)=names(state) 
    
    out= data.frame(solvemodel(params, state.i)) #creates table of model output
    out1 = out[match(data.compare.i$time, out$time),c(1,11)] #these columns need to match the ones that were pulled out before
    lat = rep(lat.i, length(out1[,1]))
    out1 = cbind(Latitude = lat, out1)
    
    out.compare1 = rbind(out.compare1, out1)
  } #end of latitudes loop    
  
      error.time=matrix(0, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
      for (d in 1:D) { #for each data type
        for (m in 1:length(data.compare1$time)){ #for each timestep
          if(!is.na(data.compare1[m,d+2])){ #if there is data at that timestep for that data stream
            error.time[m,d]=((data.compare1[m,d+2] - out.compare1[m,d+2])/sigma.obs1[m,d+2])^2 #calculates the error at that timestep for that data stream
          } #end of if statement
          #if there was no data at that timestep, the error will remain "0" so that it will not impact the sum calculation in the next step
        } #end of time step loop
        
        j[i,d] = sum(error.time[,d]) #calculate cost function for each data stream
        
      } #end of data type loop
      
      J[i] = prod(j[i,]) #calculate aggregate cost function
      
      
      diff = J[i-1]/J[i] #calculate the acceptance ratio
      
      if(is.na(diff)){
        reject = reject+1 #reject parameter set
        param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
        J[i] = J[i-1] #set current J to previous J
        j[i,] = j[i-1,]
      } else { 
      
      if(diff<1){ #if difference is < 1 (or if the current J is greater than the previous J)
        
        u=runif(1, 0, 1) #draw random number between 0 and 1
        
        if(u>=diff){   
          reject = reject+1 #reject parameter set
          param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
          J[i] = J[i-1] #set current J to previous J (the minimum J so far)
          j[i,] = j[i-1,]
        } #end of if loop
      } #end of if loop
      
    } #end of else loop
    
    acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
    
    if(acceptance>0.30){
      t = 1.01*t
    }
    
    if(acceptance<0.25){
      t = 0.99*t
    }
  
    if(t>0.5){
      t=0.5
    }
    
  
  
} #end of exploration


plot(all.draws[1:i,1])
lines(param.est[1:i,1], col="red")


steps=seq(1:i) #create a vector that represents the number of steps or iterations run
J1=data.frame(steps, J[1:i]) #create a dataframe that has "steps" as the first column and "J" as the second column
head(J1); tail(J1) #check the table
step.best = J1[which.min(J1[,2]),1] #determine which step has the minimum value of J and store as "step.best"
param.est[step.best,] #show the parameter set that resulted in the best J
param.best = as.numeric(param.est[step.best,]) #store that parameter set as param.best
names(param.best) = c(names(params)) #change the names to match params
j.best = j[step.best,] #pull out the minimum j
param.best #view the best parameter set
state
j.best #view the minimum J
i
t
acceptance

save.image(file="Step1_082016_ALPHA.Rdata")
