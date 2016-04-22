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


####DATA EXPLORATION###
#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(0.34,0.0024,0.012,0.23,0.022,0.04,3,   941,17,24217,1045,3.62)
param.min=c(0.07,0.0001,0.002,0.01,0.01,0.003,1,  408,8,14501,663,0.03)

##STEP 1: Explore with BOTH NEE and NDVI

#other necessary knowns
n.param = length(params)#number of parameters to estimate
M = 200000 #number of iterations
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
param.est[1,]=params #change first row to current guess
all.draws[1,]=params #change first row to current guess
colnames(param.est) = c(names(params))
head(param.est) #check to make sure this is correct
head(all.draws)

#replace 1st row with values for current parameters
out=data.frame(solvemodel(params))
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
anneal.temp0=10000000 #starting temperature
anneal.temp=10000000 #starting temperature
reject=0 #reset reject counter
t=0.5


for (i in 46012:M) {
  
  repeat{
    for(p in 1:n.param){ #for each parameter
      param.est[i,p] = param.est[i-1,p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
      all.draws[i,p] = param.est[i,p]
    } #end of parameter loop
    if(all(!is.na(param.est[i,]))){
      if(all(param.est[i,]>=param.min) & all(param.est[i,]<=param.max)){
        break
      } #end of if loop
    } #end of if loop
  } #end of repeat
  
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
    
    if(acceptance>0.20){
      t = 1.01*t
    }
    
    if(acceptance<0.05){
      t = 0.99*t
    }
    
    
    anneal.temp=anneal.temp*0.999 #decrease temperature
    
    
    
    if(anneal.temp<(0.01*anneal.temp0)){ #if temperature drops to less than 10% of initial
      anneal.temp=anneal.temp0 #jump back up to initial
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
j.best #view the minimum J

save.image(file="Step1_041916.Rdata")
