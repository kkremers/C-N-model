######Synthetic data experiments######

#Get data ready
head(out)
data.assim = out[,c(1:8, 10,11)]
head(data.assim)

#add some noise to the data
for (i in 1:1826){
  for (j in 2:length(data.assim)){
    data.assim[i,j] = data.assim[i,j] + rnorm(1, 0, abs((data.assim[i,j]/100)))
    
  } 
}

#remove some data points
time.keep  = seq(1, length(time), 15) #keep data for every 15 days
data.assim = data.assim[match(time.keep, data.assim$time),] 
head(data.assim)

#plot to view data
par(mfrow=c(3,2))
head(data.assim)
plot(data.assim$Biomass_C~data.assim$time, pch=16, ylab="Biomass_C", xlab="Time (days)")
plot(data.assim$Biomass_N~data.assim$time, pch=16, ylab="Biomass_N", xlab="Time (days)")
plot(data.assim$Litter_C~data.assim$time, pch=16, ylab="Litter_C", xlab="Time (days)")
plot(data.assim$Litter_N~data.assim$time, pch=16, ylab="Litter_N", xlab="Time (days)")
plot(data.assim$SOM_C~data.assim$time, pch=16, ylab="SOM_C", xlab="Time (days)")
plot(data.assim$SOM_N~data.assim$time, pch=16, ylab="SOM_N", xlab="Time (days)")
plot(data.assim$Available_N~data.assim$time, pch=16, ylab="Available_N", xlab="Time (days)")
plot(data.assim$LAI~data.assim$time, pch=16, ylab="LAI", xlab="Time (days)")
plot(data.assim$NEE~data.assim$time, pch=16, ylab="NEE", xlab="Time (days)")



data.compare1 = data.assim[,c(1,9,10)] #pull out columns for data that you want to assimilate
sigma.obs1 = data.frame(matrix(1, length(data.compare1$time), length(data.compare1))) #observation errors for each data type 
sigma.obs1[,1] = data.assim$time
colnames(sigma.obs1) = colnames(data.compare1)
#sigma.obs1: columns need to be in SAME ORDER as data.compare1
head(data.compare1)
head(sigma.obs1)


###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = 16 #number of parameters
M = 100 #number of iterations
D = length(data.compare1)-1 #number of data types being assimilated (number of columns in data.compare1, minus the "time" column)

#storage matrices
J = rep(100000000000, M) #storage vector for cost function output
j=matrix(0, M, D) #to store error calculations for this iteration
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
param.est[,11] = params[11]
param.est[,12] = params[12]
param.est[,13] = params[13]
param.est[,14] = params[14]
param.est[,15] = params[15]
param.est[,16] = params[16]


head(param.est) #check to make sure this is correct

#set up vectors with min and max values for each parameter
param.max=c(1, 0.01, 0.1, 0.1, 1, 0.1, 0.9, 0.1, 0.1, 700, 20, 600, 10, 5000, 150, 10)
param.min=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 2, 50, 1, 800, 20, 0.01)

#set t to initial value
t = 0.5
reject=0 #reset reject counter

#start exploration
for (i in 2:M) { #for each iteration
  
  #draw a parameter set from proposal distribution
  for(p in 1:n.param){ #for each parameter
    repeat { #repeat until proposed parameter is within specified range
      r = runif(1, -0.5*t, 0.5*t) #draw value of r between +/- 0.5*t
      param.est[i,p] = param.est[i-1,p] + r*(param.max[p]-param.min[p]) #draw new parameter set
      if(param.est[i,p]>param.min[p] && param.est[i,p]<param.max[p]){ #if the proposed parameter is in the specified range
        break #break the repeat loop
      }#end of if loop
    } #end of repreat loop
  } #end of parameter loop
  
  
  #run model and calculate error function 
  parms = as.numeric(param.est[i,]) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms)) #run model
  #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
  out.compare1 = out[match(data.compare1$time, out$time),c(1,9,10)] #these columns need to match the ones that were pulled out before
  
  
  for (d in 1:D) { #for each data type
    
    j[i,d] = sum(((data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[,d+1])^2)/length(data.compare1[1,d+1]) #calculate uncertainty weighted error term
    
  } #end of data type loop
  
  J[i] = sum(j[i,])/D #calculate aggregate cost function
  
  tnew = NULL
  if(J[i]>J[i-1]){ #if current J is greater than previous J
    reject = reject +1 #reject parameter set
    param.est[i,] = param.est[i-1,] #set current parameter set to previous one
    J[i] = J[i-1] #set current J to previous J (the minimum J so far)
    tnew = 0.99*t #decrease the size of the parameter space
  } else { #if parameter set is accepted
    tnew=1.01*t #increase t 
  } 
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  
  #if the acceptance rate is greater than 50% or less than 30% adjust the t value accordingly
  if(acceptance > 0.5) {
    t = tnew
  }
  
  if (acceptance < 0.3) {
    t = tnew
  }
  
  
} #end of exploration


#The final iteration should be the smallest J
min(J)
J[M] #last element in this should match min(J)
param.est[M,] #final parameter set will be the one that resulted in the smallest J


###STEP 2: MCMC

#storage matrices
J = rep(1, M) #storage vector for cost function output
j=matrix(0, M, D) #to store error calculations for this iteration
param.est = data.frame(matrix(1, M, n.param)) #storage for parameter estimate iterations 
colnames(param.est) = c(names(params)) #, names(state))
#change values to the starting values (the parameters that resulted in the smallest J in step 1)
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
param.est[,11] = params[11]
param.est[,12] = params[12]
param.est[,13] = params[13]
param.est[,14] = params[14]
param.est[,15] = params[15]
param.est[,16] = params[16]


head(param.est) #check to make sure this is correct










reject = 0 #reset rejection counter

#start MCMC
print(system.time( #prints the amount of time the MCMC took to run
  for (i in 2:M) { #for each iteration
    
    #draw a parameter set from proposal distribution
    ###########NEED TO DO THIS############
    
    
    #run model and calculate error function 
    parms = as.numeric(param.est[i,]) #parameters for model run
    names(parms) = names(params) #fix names
    out = data.frame(solvemodel(parms)) #run model
    #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
    out.compare1 = out[match(data.compare1$time, out$time),c(1,9,10)] #these columns need to match the ones that were pulled out to create assimilation data 
    
    
    for (d in 1:D) { #for each data type
      
      j[i,d] = sum(((data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[,d+1])^2) #calculate uncertainty weighted error term
      
    } #end of data type loop
    
    J[i] = prod(j[i,]) #calculate aggregate cost function
    
    if (J[i] == "NaN"){
      reject = reject+1 #add to number of rejections
      param.est[i,] = param.est[i-1,] #set parameter values to previous set
      J[i] = J[i-1]
    }
    
    if(J[i] != "NaN"){
      if (J[i] == "Inf"){
        reject = reject+1 #add to number of rejections
        param.est[i,] = param.est[i-1,] #set parameter values to previous set
      }
      
      if (J[i] != "Inf"){
        #calculate likelihood ratio
        ratio = exp(-J[i]+J[i-1])
        
        u = runif(1, 0, 1)
        
        if(ratio < u){
          reject = reject+1 #add to number of rejections
          param.est[i,] = param.est[i-1,] #set parameter values to previous set
        }  
      }
    } 
  }))


#check trace plots
head(param.est)
tail(param.est)
plot(param.est[,1], type="l")






















#############################NOT SURE IF THIS IS RIGHT##############
require(MCMCpack)
###STEPS FOR MH algorithm##
#1. Specify starting values
#2. Draw candidate parameter from proposal distribution
#3. Compute acceptance ratio
#4. draw u ~unif(0,1)
#5. accept or reject
#6. repeat from step 2


#necessary knowns
n.param = 17 #number of parameters
M = 1000 #number of iterations
D = 2 #number of data types being assimilated (4xflux, 7xplant/soil)

#set up storage matrices
theta = data.frame(matrix(1, M, n.param)) 
colnames(theta) = c(names(params)) 
theta[1,]= unlist(parms) #set starting values to best parameter set from exploration step
tau = data.frame(matrix(0.1, M, n.param)) 
s = data.frame(matrix(0.1, M, 2))


param.est = data.frame(matrix(1, M, n.param)) #storage for CCaN parameter estimate iterations
colnames(param.est) = c(names(params)) 
sigma = data.frame(matrix(1, M, 1)) #storage for sigma 

#start MCMC
print(system.time( #prints the amount of time the MCMC took to run
  for (i in 2:M) { #for each iteration
    
    #draw a hyper-parameter set from proposal distribution
    s[i,1] = runif(1, s[i-1,1]-0.1), s[i-1,1]+0.1)
  s[i,2] = runif(1, s[i-1,2]-0.1), s[i-1,2]+0.1)
for (p in 1:n.param){
  theta[i,p] = runif(1, theta[i-1,p]-0.1), theta[i-1,p]+0.1)
tau[i,p] = runif(1, tau[i-1,p]-0.1), tau[i-1,p]+0.1)
} 

#draw parameters
sigma[i,1] = rinvgamma(1, s[i,1], s[i,2])
for(p in 1:n.param){
  param.est[i,p] = rnorm(1, theta[i,p], tau[i,p])
}

#calculate ratio
parms1 = as.numeric(param.est[i,]) #parameters for model run
names(parms1) = names(params) #fix names
out1 = data.frame(solvemodel(parms1))     
out1 = out1[,1:8]    #pull out columns to compare

parms0 = as.numeric(param.est[i-1,]) #parameters for model run
names(parms0) = names(params) #fix names
out0 = data.frame(solvemodel(parms0))
out0 = out0[,1:8] #pull out columns to compare

#calculate likelihood for EACH data type

#multiply likelihoods

#draw u
#accept or reject

  }))
