
##########################NEE AND NDVI###############################

data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = subset(data.assim, Year==2009 | Year==2011 | Year==2013)
data.sigma = subset(data.sigma, Year==2009 | Year==2011 | Year==2013)
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,9]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,9]))
head(data.compare1)
head(sigma.obs1)

###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = length(params) #number of parameters to estimate
M = 50000 #number of iterations
D = 2 #number of data types being assimilated 
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 


#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(0.34,0.0024,0.004,0.92,0.03,0.04,  820,15,22000,950,3)
param.min=c(0.07,0.0001,0.002,0.09,0.002,0.001,  550,10,16500,750,0.5)

#storage matrices
J = rep(1E100, M) #storage vector for cost function output
j = matrix(1E100, M, D) #to store error calculations for this iteration
all.draws = data.frame(matrix(1, M, n.param)) #storage for all parameter estimate iterations;
colnames(all.draws) = c(names(params))
param.est = data.frame(matrix(1, M, n.param)) #storage for accepted parameter estimate iterations;
param.est[1,]=param.best #change first row to current guess
all.draws[1,]=param.best #change first row to current guess
colnames(param.est) = c(names(params))
head(param.est) #check to make sure this is correct
head(all.draws)

#replace 1st row with values for current parameters
out=data.frame(solvemodel(param.best))
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
anneal.temp0=50000 #starting temperature
anneal.temp=50000 #starting temperature
iter=1 #simulated annealing iteration counter
reject=0 #reset reject counter
t=0.5

#start exploration

for (i in 2:M) {
  
  for(p in 1:n.param){ #for each parameter
    param.est[i,p] = param.est[i,p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
    if(param.est[i,p] > param.max[p]){param.est[i,p]=param.max[p]}
    if(param.est[i,p] < param.min[p]){param.est[i,p]=param.min[p]}
    if(is.na(param.est[i,p])){param.est[i,p]=(param.max[p] + param.min[p])/2}
    all.draws[i,p] = param.est[i,p]
  } #end of parameter loop
  
  parms = as.numeric(param.est[i,]) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms)) #run model  
  
  if(any(is.na(out)) | any(out[,2:6]<0) | abs(out[1,2]-out[length(out[,2]),2])>100){ #if there are any NAs or negative stocks in the output
    reject = reject+1 #reject parameter set
    param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
    J[i] = J[i-1] #set current J to previous J
  } else { #if there are no NAs or negative stocks & biomass pool doesn't crash
    
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
    
    
    diff=J[i]-J[i-1] #calculate difference between current J and previous J
    
    if(diff>0){ #if difference is > 0 (or if the current J is greater than the previous J)
      
      u=runif(1, 0, 1) #draw random number between 0 and 1
      prob=exp((-1*diff)/anneal.temp) #simulated annealing - determines probability that a parameter set is accepted
      
      if(u>=prob){    
        reject = reject+1 #reject parameter set
        param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
        J[i] = J[i-1] #set current J to previous J (the minimum J so far)
      } #end of if loop
    } #end of if loop
    
  } #end of else loop
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  if(acceptance>0.30){
    t = 1.01*t
  }
  
  if(acceptance<0.20){
    t = 0.99*t
  }
  
  if(t>1){
    t=1
  }
  
  anneal.temp=anneal.temp*0.9 #decrease temperature
  
  
  
  if(anneal.temp<(0.1*anneal.temp0)){ #if temperature drops to less than 10% of initial
    anneal.temp=anneal.temp0 #jump back up to initial
  }
  
  
} #end of exploration

#beep(5)
#make plots to check for mixing and make sure parameter space is thuroughly explored
plot(all.draws[1:i,2])
lines(param.est[1:i,2], col="red", lwd="2")

steps=seq(1:i) #create a vector that represents the number of steps or iterations run
J1=data.frame(steps, J[1:i]) #create a dataframe that has "steps" as the first column and "J" as the second column
head(J1); tail(J1) #check the table
step.best = J1[which.min(J1[,2]),1] #determine which step has the minimum value of J and store as "step.best"
param.est[step.best,] #show the parameter set that resulted in the best J
param.best = as.numeric(param.est[step.best,]) #store that parameter set as param.best
names(param.best) = names(params) #change the names to match params
j.best = j[step.best,] #pull out the minimum j
param.best #view the best parameter set
j.best #view the minimum J

save.image(file="Step1_NEE_NDVI_part1BOTH_022816.Rdata")


#######################NDVI ONLY#############################

data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = subset(data.assim, Year==2009 | Year==2011 | Year==2013)
data.sigma = subset(data.sigma, Year==2009 | Year==2011 | Year==2013)
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NDVI=data.assim[,9]))
sigma.obs1 = data.frame(cbind(time=time.assim, NDVI=data.sigma[,9]))
head(data.compare1)
head(sigma.obs1)

###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = length(params) #number of parameters to estimate
M = 50000 #number of iterations
D = 1 #number of data types being assimilated 
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 


#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(0.34,0.0024,0.004,0.92,0.03,0.04,  820,15,22000,950,3)
param.min=c(0.07,0.0001,0.002,0.09,0.002,0.001,  550,10,16500,750,0.5)

#storage matrices
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
out=data.frame(solvemodel(params))
out.compare1 = out[match(data.compare1$time, out$time),c(1,11)] #these columns need to match the ones that were pulled out before

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


head(j)
tail(j)
head(param.est)
tail(param.est)
head(all.draws)
tail(all.draws)

#set initial values
anneal.temp0=1000 #starting temperature
anneal.temp=1000 #starting temperature
iter=1 #simulated annealing iteration counter
reject=0 #reset reject counter
t=0.5

#start exploration

for (i in 2:M) {
  
  for(p in 1:n.param){ #for each parameter
    param.est[i,p] = param.best[p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
    if(param.est[i,p] > param.max[p]){param.est[i,p]=param.max[p]}
    if(param.est[i,p] < param.min[p]){param.est[i,p]=param.min[p]}
    if(is.na(param.est[i,p])){param.est[i,p]=(param.max[p] + param.min[p])/2}
    all.draws[i,p] = param.est[i,p]
  } #end of parameter loop
  
  parms = as.numeric(param.est[i,]) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms)) #run model  
  
  if(any(is.na(out)) | any(out[,2:6]<0) | abs(out[1,2]-out[length(out[,2]),2])>100){ #if there are any NAs or negative stocks in the output
    reject = reject+1 #reject parameter set
    param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
    j[i,] = j[i-1,] #set current J to previous J
  } else { #if there are no NAs or negative stocks & biomass pool doesn't crash
    
    #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
    
    out.compare1 = out[match(data.compare1$time, out$time),c(1,11)] #these columns need to match the ones that were pulled out before
    
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
    
    
    diff=j[i,]-j[i-1,] #calculate difference between current J and previous J
    
    if(diff>0){ #if difference is > 0 (or if the current J is greater than the previous J)
      
      u=runif(1, 0, 1) #draw random number between 0 and 1
      prob=exp((-1*diff)/anneal.temp) #simulated annealing - determines probability that a parameter set is accepted
      
      if(u>=prob){    
        reject = reject+1 #reject parameter set
        param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
        j[i,] = j[i-1,] #set current J to previous J (the minimum J so far)
      } #end of if loop
    } #end of if loop
    
  } #end of else loop
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  if(acceptance>0.30){
    t = 1.01*t
  }
  
  if(acceptance<0.20){
    t = 0.99*t
  }
  
  anneal.temp=anneal.temp-1 #decrease temperature
  
  if(anneal.temp<5){ #if temperature drops to less than 1
    anneal.temp=anneal.temp0 #jump back up to initial
  }
  
} #end of exploration


#beep(5)
#make plots to check for mixing and make sure parameter space is thuroughly explored
plot(all.draws[1:i,2])
lines(param.est[1:i,2], col="red", lwd="2")

step.best=which.min(j)
j.best = j[step.best,] #pull out the minimum j
param.best=as.numeric(param.est[step.best,]) #view the best parameter set
names(param.best)=names(params)
j.best #view the minimum J
param.best

save.image(file="Step1_NEE_NDVI_part2NDVI_022816.Rdata")




#######################NEE ONLY######################

data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = subset(data.assim, Year==2009 | Year==2011 | Year==2013)
data.sigma = subset(data.sigma, Year==2009 | Year==2011 | Year==2013)
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6]))
head(data.compare1)
head(sigma.obs1)
head(sigma.obs1)

###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = length(params) #number of parameters to estimate
M = 25000 #number of iterations
D = 1 #number of data types being assimilated 
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 


#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(0.34,0.0024,0.004,0.92,0.03,0.04,  820,15,22000,950,3)
param.min=c(0.07,0.0001,0.002,0.09,0.002,0.001,  550,10,16500,750,0.5)


#storage matrices
j = matrix(1E100, M, D) #to store error calculations for this iteration
all.draws = data.frame(matrix(1, M, n.param)) #storage for all parameter estimate iterations;
colnames(all.draws) = c(names(params))
param.est = data.frame(matrix(1, M, n.param)) #storage for accepted parameter estimate iterations;
param.est[1,]=param.best #change first row to current guess
all.draws[1,]=param.best #change first row to current guess
colnames(param.est) = c(names(params))
head(param.est) #check to make sure this is correct
head(all.draws)

#replace 1st row with values for current parameters
out=data.frame(solvemodel(param.best))
out.compare1 = out[match(data.compare1$time, out$time),c(1,7)] #these columns need to match the ones that were pulled out before

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


head(j)
tail(j)
head(param.est)
tail(param.est)
head(all.draws)
tail(all.draws)

#set initial values
anneal.temp0=1000 #starting temperature
anneal.temp=1000 #starting temperature
iter=1 #simulated annealing iteration counter
reject=0 #reset reject counter
t=0.5


#start exploration

for (i in 2:M) {
  
  for(p in 1:n.param){ #for each parameter
    param.est[i,p] = param.best[p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
    if(param.est[i,p] > param.max[p]){param.est[i,p]=param.max[p]}
    if(param.est[i,p] < param.min[p]){param.est[i,p]=param.min[p]}
    if(is.na(param.est[i,p])){param.est[i,p]=(param.max[p] + param.min[p])/2}
    all.draws[i,p] = param.est[i,p]
  } #end of parameter loop
  
  parms = as.numeric(param.est[i,]) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms)) #run model  
  
  if(any(is.na(out)) | any(out[,2:6]<0)| abs(out[1,2]-out[length(out[,2]),2])>100){ #if there are any NAs or negative stocks in the output
    reject = reject+1 #reject parameter set
    param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
    j[i,] = j[i-1,] #set current J to previous J
  } else { #if there are no NAs or negative stocks & biomass pool doesn't crash
    
    #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
    
    out.compare1 = out[match(data.compare1$time, out$time),c(1,7)] #these columns need to match the ones that were pulled out before
    
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
    
    
    diff=j[i,]-j[i-1,] #calculate difference between current J and previous J
    
    if(diff>0){ #if difference is > 0 (or if the current J is greater than the previous J)
      
      u=runif(1, 0, 1) #draw random number between 0 and 1
      prob=exp((-1*diff)/anneal.temp) #simulated annealing - determines probability that a parameter set is accepted
      
      if(u>=prob){    
        reject = reject+1 #reject parameter set
        param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
        j[i,] = j[i-1,] #set current J to previous J (the minimum J so far)
      } #end of if loop
    } #end of if loop
    
  } #end of else loop
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  if(acceptance>0.30){
    t = 1.01*t
  }
  
  if(acceptance<0.20){
    t = 0.99*t
  }
  
  anneal.temp=anneal.temp*0.9 #decrease temperature
  
  
  
  if(anneal.temp<(0.1*anneal.temp0)){ #if temperature drops to less than 1
    anneal.temp=anneal.temp0 #jump back up to initial
  }
  
  #step.best = which.min(j)
  #param.best = as.numeric(param.est[step.best,]) #store the parameter set that has the smallest j as param.best
  #names(param.best) = colnames(param.est)  
  
  
} #end of exploration


#beep(5)
#make plots to check for mixing and make sure parameter space is thuroughly explored
plot(all.draws[1:i,2])
lines(param.est[1:i,2], col="red", lwd="2")
step.best = which.min(j)
param.best = as.numeric(param.est[step.best,]) #store the parameter set that has the smallest j as param.best
names(param.best) = colnames(param.est)  
j.best = j[step.best,] #pull out the minimum j
param.best #view the best parameter set
j.best #view the minimum J

save.image(file="Step1_NEE_NDVI_part3NEE_022816.Rdata")


##########################################################

##########STEP 2 PARAMETER UNCERTAINTY####################
#load packages
require(deSolve)

#######STEP 2: ESTIMATE PARAMETER UNCERTAINTY

#need to calculate the variance of the errors for the minimum j's
head(data.assim)
head(data.sigma)
out = data.frame(solvemodel(param.best)) #creates table of model output
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,9]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,9]))
#pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
out.compare1 = out1[match(data.compare1$time, out1$time),c(1,7,11)] #these columns need to match the ones that were pulled out before
head(out.compare1)
head(data.compare1)
head(sigma.obs1)

#create storage matrices for error and variance
n.param = length(params) #number of parameters to estimate
D = 2 #number of data types being assimilated 
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 

var.jbest = rep(0, D)
error.jbest=matrix(NA, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
for (d in 1:D) { #for each data type
  for (m in 1:length(data.compare1$time)){ #for each timestep
    if(!is.na(data.compare1[m,d+1])){ #if there is data at that timestep for that data stream
      error.jbest[m,d]=((data.compare1[m,d+1] - out.compare1[m,d+1])/sigma.obs1[m,d+1])^2 #calculates the error at that timestep for that data stream
    } #end of if statement
  } #end of time step loop
  
  var.jbest[d] = var(error.jbest[!is.na(data.compare1[,d+1]),d]) #calculate variance of the errors (excludes NAs)
  
} #end of data type loop

var.jbest #preview

#storage matrices for Monte Carlo reps
j = rep(0, D)
param.keep = data.frame(matrix(1, 1000, n.param)) #storage for parameter estimate iterations; 
colnames(param.keep) = c(names(param.best))
param.keep[1,]=param.best
head(param.keep)#check to make sure this is correct


#also need to know degrees of freedom for chi square test
n.par = length(params) #number of parameters predicted by each data stream
df = rep(0, D)
for (d in 1:D) { #for each data type
  df[d] = n.time[d] - n.par
} #end of data loop
df #check values

#set initial values
param.est = param.best #set initial values for parameters
reject=0 #reset reject counter
num.accepted = 0 #counter for number of accepted parameters - when this gets to 1000, loop will stop
num.reps = 0 #counter for number of repititions - calculates acceptance rate
t=t 
#start loop
repeat { #repeat until desired number of parameter sets are accepted
  
  num.reps=num.reps+1 #add to number of reps counter
  
  repeat{
    for(p in 1:n.param){ #for each parameter
      step.size = t*(param.max[p]-param.min[p])
      param.est[p] = param.best[p]+rnorm(1, 0, step.size) #draw new parameter set
    } #end of parameter loop 
    if(all(param.est>param.min) & all(param.est<param.max)){    
      break
    } #end of if loop
  }#end of repeat loop
  
  parms = as.numeric(param.est) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms)) #run model
  
  if(any(is.na(out)) | any(out[,2:6]<0)){ #if there are NAs or negative stocks in the output
    reject=reject+1
  } else {
    
    #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
    out.compare1 = out[match(data.compare1$time, out$time),c(1,7,11)] #these columns need to match the ones that were pulled out before
    
    #remove the time column - no longer needed
    data.comp = data.compare1[,-1]
    out.comp = out.compare1[,-1]
    sigma = sigma.obs1[,-1]
    
    #determine if parameter set is accepted or rejected
    error = matrix(NA, length(data.comp[,1]), D)
    var.error=rep(0,D)  
    error=((data.comp - out.comp)/sigma)^2 #calculates the error at that timestep for that data stream
    
    for (d in 1:D) { #for each data type
      
      var.error[d] = var(error[!is.na(data.comp[,d]),d]) #calculate variance of the errors (excludes NAs)
      
      for (m in 1:length(data.comp[,1])){ #for each timestep
        error[m,d] = (error[m,d]*sqrt(var.jbest[d]))/sqrt(var.error[d]) #variance normalization
      } #end of time step loop  
      
      j[d] = sum(error[!is.na(data.comp[,d]),d]) #calculate cost function for each data stream after variance normalizaiton
    } #end of data type loop
    
    #chi-square test
    accept = rep (0, D) #vector to keep track of if each j has been accepted or rejected; 1=accept, 0=reject
    for (d in 1:D) { #for each data type  
      
      if(j[d]-j.best[d] <= qchisq(0.9, df[d])) { #conduct chi square test
        accept[d] = 1} #if accepted, change value in accept vector to 1
    } #end of data type loop
    
    d.accept = sum(accept) #calculate the number of j's accepted
    
    if(d.accept==D) { #if all j's are accepted
      num.accepted = num.accepted+1 #add to number of parameter sets accepted
      param.keep[num.accepted,]=param.est #store the parameter set in the storage dataframe
    } #end of if loop
    if(d.accept<D) { #if any j's rejected
      reject = reject+1 #reject parameter set
    } #end of if loop
    
  } #end of else loop
  
  acceptance = 1 - (reject / num.reps) #calculate proportion of accepted iterations
  
  
  #print number of accepted parameters every 10 parameters
  if(num.accepted > 10){
    if((num.accepted/10 - floor(num.accepted/10)) == 0){
      print(num.accepted)
    }  
  }
  
  if (num.accepted==1000) { #if you have accepted the number of parameter sets you want (i.e., 1000)
    break  #break repeat loop
  } 
  
} #end of repeat

#beep(5)

head(param.keep)
tail(param.keep)

save.image(file="Step2_NEE_NDVI_UNBdata_121015.Rdata")





