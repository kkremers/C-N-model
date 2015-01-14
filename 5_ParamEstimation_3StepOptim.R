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
plot(data.assim[,4])
plot(data.assim[,5])
plot(data.assim[,6])
plot(data.assim[,7]
     plot(data.assim[,8])
     plot(data.assim[,9])
     plot(data.assim[,10])
     

data.compare1 = data.assim[1:5] #pull out columns that you need
sigma.obs1 = matrix(1, length(data.compare1$time), 5) #observation erros for each data type - data frame with 288 rows and 4 columns corresponding to each data type
sigma.obs1[,1] = data.assim$time
sigma.obs1[,3] = 0.1
#FOR sigma.obs1: columns need to be in SAME ORDER as data.compare1
head(data.compare1)
head(sigma.obs1)


###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = 17 #number of parameters
M = 100000 #number of iterations
D = 4 #number of data types being assimilated (4xflux, 7xplant/soil)

#storage matrices
J = rep(1, M) #storage vector for cost function output
j=matrix(0, M, D) #to store error calculations for this iteration
param.est = data.frame(matrix(1, M, n.param)) #storage for parameter estimate iterations; 
colnames(param.est) = c(names(params)) #, names(state))
#change values to the starting values
param.est[,1] = 0.002 #LitterRate
param.est[,2] = 0.00005 #DecompRateC
param.est[,3] = 0.00008 #DecompRateN
param.est[,4] = 0.7 #retrans
param.est[,5] = 0.00001 #RespRateSOM
param.est[,6] = 0.005 #RespRateL
param.est[,7] = 0.008  #kCUE
param.est[,8] = 0.4  #CUEMax
param.est[,9] = 0.5  #kplant
param.est[,10] = 0.05 #Uptakemax
param.est[,11] = 0.3 #Available_N
param.est[,12] = 250 #Biomass_C
param.est[,13] = 4 #Biomass_N
param.est[,14] = 150 #Litter_C
param.est[,15] = 3 #Litter_N
param.est[,16] = 3000 #SOM_C
param.est[,17] = 70 #SOM_N

head(param.est) #check to make sure this is correct

#set up vectors with min and max values for each parameter
param.max=c(0.05, 0.001, 0.001, 0.99, 0.001, 0.01, 0.1, 0.9, 1, 1, 10, 1000, 10, 1000, 10, 5000, 200)
param.min=c(0, 0, 0, 0, 0, 0, 0, 0.3, 0, 0.1, 0.05, 100, 1, 20, 0.2, 1000, 20)

#set t to initial value
t = 1
reject=0 #reset reject counter

print(system.time( #prints the amount of time the MCMC took to run
  for (i in 2:M) { #for each iteration
    
    #draw a parameter set from proposal distribution
    for(p in 1:n.param){ #for each parameter
      repeat { #repeat until proposed parameter is within specified range
      param.est[i,p] = param.est[i-1,p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
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
    out.compare1 = out[match(data.compare1$time, out$time),c(1:8)] 
    
    
    for (d in 1:D) { #for each data type
      
      j[i,d] = sum(((data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[,d+1])^2)/length(data.compare1[1,d+1]) #calculate uncertainty weighted error term
      
    } #end of data type loop
    
    J[i] = sum(j[i,])/D #calculate aggregate cost function
    
    if(J[i]>J[i-1]){
      reject = reject +1
      param.est[i,] = param.est[i-1,]
      t = 0.99*t
    } else {
      t = 1.01*t
    }
    
    
  })) #end of exploration
  

plot(param.est[10000:25000,17], type="l")
plot(density(param.est[10000:25000,17]))

###STEP 2: MCMC
#necessary knowns
n.param = 17 #number of parameters
M = 10000 #number of iterations
D = 7 #number of data types being assimilated

#storage matrices
J = rep(1, M) #storage vector for cost function output
j=matrix(0, M, D) #to store error calculations for this iteration
param.est = data.frame(matrix(1, M, n.param)) #storage for CCaN parameter estimate iterations
param.est[1,] = parms.opt
colnames(param.est) = c(names(params)) 

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
  out.compare1 = out[match(data.compare1$time, out$time),c(1:8)] 
  
  
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
