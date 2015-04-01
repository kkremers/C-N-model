#load packages and workspace
install.packages("deSolve")
require(deSolve)

load("Workspace040115.Rdata")

###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = 10 #number of parameters
M = 100000 #number of iterations
D = length(data.compare1)-1 #number of data types being assimilated (number of columns in data.compare1, minus the "time" column)
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 

#storage matrices
J = rep(1, M) #storage vector for cost function output
j=matrix(1, M, D) #to store error calculations for this iteration
param.est = data.frame(matrix(1, M, n.param)) #storage for parameter estimate iterations; 
colnames(param.est) = c(names(params)) #, names(state))
#change values to the starting values
param.est[,1] = params[1]
param.est[,2] = params[2]
param.est[,3] = params[3]
param.est[,4] = params[4]
param.est[,5] = params[5]
param.est[,6] = params[6]
param.est[,7] = params[7]
param.est[,8] = params[8]
param.est[,9] = params[9]
param.est[,10] = params[10]

head(param.est) #check to make sure this is correct

#starting values for states
state <- c(Biomass_C = 400, 
           Biomass_N = 4.75, 
           Litter_C = 100, 
           Litter_N = 1.6, 
           SOM_C = 2000, 
           SOM_N = 56,
           Available_N = 0.1)

#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(1, 0.01, 0.1, 0.1, 1, 0.1, 0.9, 0.1, 0.1, 4)
param.min=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)


#set t to initial value
t = 0.5  # t is used to adjust the step size to keep acceptance rate at 50 % +/- 2.5% -- helps with mixing
anneal.temp0=12000 #starting temperature
anneal.temp=12000 #starting temperature
iter=1 #simulated annealing iteration counter
reject=0 #reset reject counter

#start exploration
for (i in 2:M) { #for each iteration
  
  #draw a parameter set from proposal distribution
  for(p in 1:n.param){ #for each parameter
    repeat { #repeat until proposed parameter is within specified range
      step.size = t*(param.max[p]-param.min[p]) #step size is a fraction of the inital parameter range
      param.est[i,p] = param.est[i-1,p] +  rnorm(1, 0, step.size) #draw new parameter set
      if(param.est[i,p]>param.min[p] && param.est[i,p]<param.max[p]){ #if the proposed parameter is in the specified range
        break #break the repeat loop
      }#end of if loop
    } #end of repreat loop
  } #end of parameter loop
  
  
  #run model and calculate error function 
  parms = as.numeric(param.est[i,]) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms, state)) #run model
  #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
  out.compare1 = out[match(data.compare1$time, out$time),c(1,10,11)] #these columns need to match the ones that were pulled out before
  
  error.time=matrix(0, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
  for (d in 1:D) { #for each data type
    for (m in 1:length(data.compare1$time)){ #for each timestep
      if(!is.na(data.compare1[m,d+1])){ #if there is data at that timestep for that data stream
        error.time[m,d]=((data.compare1[m,d+1] - out.compare1[m,d+1])/sigma.obs1[m,d+1])^2 #calculates the error at that timestep for that data stream
      } #end of if statement
      #if there was no data at that timestep, the error will remain "0" so that it will not impact the sum calculation in the next step
    } #end of time step loop
    
    j[i,d] = sum(error.time[,d])/n.time[d] #calculate uncertainty weighted error term
    if(is.na(j[i,d])) { #If it's NaN (only occurs if the model parameters were so far off that there were NaNs in the model output)
      j[i,d]=999999999999 #make the cost function a HUGE number
    }
    
  } #end of data type loop
  
  J[i] = sum(j[i,])/D #calculate aggregate cost function
  
  tnew = NULL
  
  diff=J[i]-J[i-1] #calculate probability that proposed parameter is accepted
  
  
  if(diff>0){ #if difference is > 0 (or if the current J is greater than the previous J)
    
    u=runif(1, 0, 1) #draw random number between 0 and 1
    prob=exp((-1*diff)/anneal.temp) #simulated annealing
    
    if(u>=prob){    
      reject = reject+1 #reject parameter set
      param.est[i,] = param.est[i-1,] #set current parameter set to previous one
      J[i] = J[i-1] #set current J to previous J (the minimum J so far) - This makes it easier to find the minimum J at the end of the MCMC - it will always be the last value
      anneal.temp=anneal.temp0-(1*iter) #decrease temperature
      tnew = 0.9*t #decrease the size of the parameter space
    } else { #if u<prob (accept)
      tnew=1.1*t #increase t 
    } 
  }
  
  if (diff<=0) {#accept all of these because current J is smaller than previous J
    tnew=1.1*t #increase t
  } #end of if diff
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  #If the acceptance rate is not 20% +/- 2.5%, then adjust "t"
  if(acceptance > 0.275) {
    t = tnew
  } 
  if (acceptance < 0.225) {
    t = tnew
  }
  
  iter=iter+1 #increase number of iterations counter
  
  if(anneal.temp<100){ #if temperature drops to less than 100
    anneal.temp0=(9/10)*anneal.temp0 #change initial temp to 9/10 of previous initial
    anneal.temp=anneal.temp0 #jump back up to that temp
    iter=1 #reset iteration counter
  }
  
  
} #end of exploration


save.image(file="CRCoutput040115.Rdata")
