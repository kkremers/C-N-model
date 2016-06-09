###LOAD REAL DATA###
data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = subset(data.assim, Year==2009 | Year==2011 | Year==2013 | Year==2015)
data.sigma = subset(data.sigma, Year==2009 | Year==2011 | Year==2013 | Year==2015)
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,7]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,7]))
head(data.compare1)
head(sigma.obs1)


#create dataframe for model spinup

#Step 2: calculate decadal averages
Temp.spin= tapply(data$Temp_ARF, data$DOY, mean)
plot(Temp.spin)
AvgTempGS = mean(data$Temp_avg)
TempAvg.spin = rep(AvgTempGS, 366)
PAR.spin = tapply(data$PAR_ARF, data$DOY, mean)
plot(PAR.spin)
DOY.spin=seq(1:366)
Year.spin=rep(2000,366)
data.spin=data.frame(Year=Year.spin, DOY=DOY.spin, Temp=Temp.spin, Temp_avg=TempAvg.spin, PAR=PAR.spin)
head(data.spin)

#seasonality scalar
sen.day=round(mean(data$senDOY,0))
sen.day #DOY of senescence 
num.days = 366
senDOY = rep(sen.day, num.days)
data.spin = data.frame(data.spin, senDOY = senDOY)
start.day=round(mean(data$startDOY,0))
start.day #start day
startDOY = rep(start.day, num.days)
data.spin = data.frame(data.spin, startDOY = startDOY)
end.day=round(mean(data$endDOY,0))
end.day #end day
endDOY = rep(end.day, num.days)
data.spin = data.frame(data.spin, endDOY = endDOY)
head(data.spin)


#create scalar
scal.seas.spin=rep(1, length(data.spin$DOY))
for (i in 1:length(data.spin$DOY)){
  if(data.spin$DOY[i]<data.spin$startDOY[i]){ #prior to snow melt
    scal.seas.spin[i]=0
  }
  if(data.spin$DOY[i]>=data.spin$startDOY[i]){ #after melt
    if(data.spin$DOY[i]<=data.spin$senDOY[i]){ #prior to peak
      slope = 1/(data.spin$senDOY[i]-data.spin$startDOY[i])
      scal.seas.spin[i] = 0+(slope*(data.spin$DOY[i]-data.spin$startDOY[i]))
    }
    if(data.spin$DOY[i]>data.spin$senDOY[i] & data.spin$DOY[i]<data.spin$endDOY[i]){ #after peak but before frost
      slope = 1/(data.spin$endDOY[i]-data.spin$senDOY[i])
      scal.seas.spin[i] = 0+(slope*(data.spin$endDOY[i]-data.spin$DOY[i]))
    }
    if(data.spin$DOY[i]>=data.spin$endDOY[i]){ #after frost
      scal.seas.spin[i]=0
    }
  }
}

plot(scal.seas.spin)

#temperature scalar
Tmax = max(data.spin$Temp)
Tmin = min(data.spin$Temp)
scal.temp.spin=NULL
for (i in 1:length(data.spin$Temp)){
  scal.temp.spin[i] = (data.spin$Temp[i] - Tmin)/(Tmax-Tmin) 
}

plot(scal.temp.spin, type="l")

#run model spin up for current parameter set
numyears = 50
Year.spin = rep(data.spin$Year, numyears)
DOY.spin = rep(data.spin$DOY, numyears)
Temp.spin = rep(data.spin$Temp, numyears)
TempAvg.spin = rep(data.spin$Temp_avg, numyears)
PAR.spin = rep(data.spin$PAR, numyears)
scal.temp.spin1 = rep(scal.temp.spin, numyears)
scal.seas.spin1 = rep(scal.seas.spin, numyears)

time = seq(1:length(DOY.spin))

#Step 4: make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=Temp.spin, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin1, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=scal.seas.spin1, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)


#OPEN 3_Model.R and run it the first time
out.spin= data.frame(solvemodel(params, state)) #creates table of model output
plot(out.spin$Biomass_N)
end.time = length(out.spin[,1])
#adjust starting values
state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
            Biomass_N = out.spin$Biomass_N[end.time], 
            SOM_C = out.spin$SOM_C[end.time], 
            SOM_N = out.spin$SOM_N[end.time],
            Available_N = out.spin$Available_N[end.time])


time = seq(1:length(data$time))
#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)

out= data.frame(solvemodel(params, state)) #creates table of model output

plot(out$NEE)


####DATA EXPLORATION###
#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(0.34,0.0024,0.012,0.23,0.022,0.015,3)
param.min=c(0.07,0.0001,0.002,0.01,0.01,0.00005,1)

##STEP 1: Explore with BOTH NEE and NDVI

#other necessary knowns
n.param = length(params)#number of parameters to estimate
M = 100000 #number of iterations
D = 2 #number of data types being assimilated 
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 

#storage matrices
J = rep(1E100, M) #storage vector for cost function output
j = matrix(1E100, M, D) #to store error calculations for this iteration
all.draws = data.frame(matrix(1, M, n.param)) #storage for all parameter estimate iterations;
colnames(all.draws) = c(names(params))
param.est = data.frame(matrix(1, M, n.param)) #storage for accepted parameter estimate iterations;
state.est = data.frame(matrix(1, M, length(state)))
param.est[1,]=params #change first row to current guess
all.draws[1,]=params #change first row to current guess
state.est[1,]=state
colnames(state.est) = c(names(state))
colnames(param.est) = c(names(params))
head(param.est) #check to make sure this is correct
head(all.draws)
head(state.est)

#replace 1st row with values for current parameters
out=data.frame(solvemodel(params, state))
out.compare1 = out[match(data.compare1$time, out$time),c(1,7,11)] #these columns need to match the ones that were pulled out before

error.time=matrix(0, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
for (d in 1:D) { #for each data type
  for (m in 1:length(data.compare1$time)){ #for each timestep
    if(!is.na(data.compare1[m,d+1])){ #if there is data at that timestep for that data stream
      error.time[m,d]=((data.compare1[m,d+1] - out.compare1[m,d+1])/sigma.obs1[m,d+1])^2 #calculates the error at that timestep for that data stream
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


for (i in 7313:M) {
  repeat{
    for(p in 1:n.param){ #for each parameter
      param.est[i,p] = param.est[i-1,p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
      all.draws[i,p] = param.est[i,p]
      parms = as.numeric(param.est[i,]) #parameters for model run
      names(parms) = names(params) #fix names
    } #end of parameter loop
    if(all(param.est[i,]>=param.min) & all(param.est[i,]<=param.max)){
      
      #RUN MODEL SPINUP
      time = seq(1:length(DOY.spin))
      Temp.d1 <- approxfun(x=time, y=Temp.spin, method="linear", rule=2)
      TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
      PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
      scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin1, method="linear", rule=2)
      scalseason.d1 <- approxfun(x=time, y=scal.seas.spin1, method="linear", rule=2)
      DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
      Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)
      
      state  <- c(Biomass_C = 500, 
                  Biomass_N = 10, 
                  SOM_C = 16000, 
                  SOM_N = 800,
                  Available_N = 1)
      
      out.spin= data.frame(solvemodel(parms, state)) #creates table of model output
      #adjust starting values
      state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
                  Biomass_N = out.spin$Biomass_N[end.time], 
                  SOM_C = out.spin$SOM_C[end.time], 
                  SOM_N = out.spin$SOM_N[end.time],
                  Available_N = out.spin$Available_N[end.time])
      
      if(all(!is.na(state))){ 
        break
      } #end of if loop
    } #end of if loop
  } #end of repeat
    
    state.est[i,] = state
  
    time = seq(1:length(data$time))
    Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
    TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
    PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
    scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
    scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
    DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
    Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)
    
    out = data.frame(solvemodel(parms, state)) #run model  
    
      #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
      
      out.compare1 = out[match(data.compare1$time, out$time),c(1,7,11)] #these columns need to match the ones that were pulled out before
      
      error.time=matrix(0, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
      for (d in 1:D) { #for each data type
        for (m in 1:length(data.compare1$time)){ #for each timestep
          if(!is.na(data.compare1[m,d+1])){ #if there is data at that timestep for that data stream
            error.time[m,d]=((data.compare1[m,d+1] - out.compare1[m,d+1])/sigma.obs1[m,d+1])^2 #calculates the error at that timestep for that data stream
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
        state.est[i,] = state.est[i-1,] #set states to previous states
        J[i] = J[i-1] #set current J to previous J
        j[i,] = j[i-1,]
      } else { 
      
      if(diff<1){ #if difference is < 1 (or if the current J is greater than the previous J)
        
        u=runif(1, 0, 1) #draw random number between 0 and 1
        
        if(u>=diff){   
          reject = reject+1 #reject parameter set
          param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
          state.est[i,] = state.est[i-1,] #set states to previous states
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
state.best = as.numeric(state.est[step.best,])
names(state.best) = c(names(state))
j.best = j[step.best,] #pull out the minimum j
param.best #view the best parameter set
state.best
j.best #view the minimum J
i
t
acceptance

save.image(file="Step1_060916_SPIN.Rdata")
