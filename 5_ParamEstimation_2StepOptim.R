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
M = 10 #number of iterations
D = length(data.compare1)-1 #number of data types being assimilated (number of columns in data.compare1, minus the "time" column)
n.time = length(data.compare1$time)

#storage matrices
J = rep(999999999999, M) #storage vector for cost function output
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

#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(1, 0.01, 0.1, 0.1, 1, 0.1, 0.9, 0.1, 0.1, 700, 20, 600, 10, 5000, 150, 10)
param.min=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 200, 2, 50, 1, 800, 20, 0.01)

#set t to initial value
t = 0.5   # t is used to adjust the step size to keep acceptance rate at 50 % +/- 2.5% -- helps with mixing
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
    
    j[i,d] = sum(((data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[,d+1])^2)/n.time #calculate uncertainty weighted error term
    
  } #end of data type loop
  
  J[i] = sum(j[i,])/D #calculate aggregate cost function
  
  tnew = NULL
  if(J[i]>J[i-1]){ #if current J is greater than previous J
    reject = reject +1 #reject parameter set
    param.est[i,] = param.est[i-1,] #set current parameter set to previous one
    J[i] = J[i-1] #set current J to previous J (the minimum J so far) - This makes it easier to find the minimum J at the end of the MCMC - it will always be the last value
    tnew = 0.99*t #decrease the size of the parameter space
  } else { #if parameter set is accepted
    tnew=1.01*t #increase t 
  } 
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  #If the acceptance rate is 50% +/- 2.5%, then DON'T adjust "t"
  if(acceptance > 0.525) {
    t = tnew
  } 
  if (acceptance < 0.475) {
    t = tnew
  }
  
  
} #end of exploration


#The final iteration should be the smallest J
min(J)
J[M] #last element in this should match min(J)
param.est[M,] #final parameter set will be the one that resulted in the smallest J
params.best = as.numeric(param.est[M,])
names(params.best) = names(params)
j.best = j[M,]
params.best
j.best

###STEP 2: MCMC

#need to calculate the variance of the errors for the minimum j's

out = data.frame(solvemodel(params.best)) #run model
#pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
out.compare1 = out[match(data.compare1$time, out$time),c(1,9,10)] #these columns need to match the ones that were pulled out before

error.jbest = matrix(0, n.time, D)
var.jbest = rep(0, D)

for (d in 1:D) { #for each data type
  for (m in 1:n.time){
  error.jbest[m,d] = (data.compare1[m,d+1] - out.compare1[m,d+1])/sigma.obs1[m,d+1]
  }
  var.jbest[d] = var(error.jbest[,d])
}

var.jbest

#storage matrices
j = rep(0, D)
error=matrix(0, n.time, D)
param.keep = NULL

#set inital values
param.est = params.best #storage for parameter estimate iterations 
param.est #check to make sure this is correct
t = t #keep "t" at the same value as it was at the end of exploration step
num.accepted = 0

#also need to know degrees of freedom for chi square test
df = rep(0, D)
for (d in 1:D) { #for each data type
  df[d] = length(data.compare1[,d+1]) - n.param
} #end of data loop
df #check values

#start loop
repeat { #repeat until we have accepted 1000 parameter sets
  
  #draw a parameter set from proposal distribution
  for(p in 1:n.param){ #for each parameter
    repeat { #repeat until proposed parameter is within specified range
      r = runif(1, -0.5*t, 0.5*t) #draw value of r between +/- 0.5*t
      param.est[p] = param.est[p] + r*(param.max[p]-param.min[p]) #draw new parameter set
      if(param.est[p]>param.min[p] && param.est[p]<param.max[p]){ #if the proposed parameter is in the specified range
        break #break the repeat loop
      }#end of if loop
    } #end of repreat loop
  } #end of parameter loop
  
  
  #run model and calculate error function 
  parms = as.numeric(param.est) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms)) #run model
  #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
  out.compare1 = out[match(data.compare1$time, out$time),c(1,9,10)] #these columns need to match the ones that were pulled out before
  
  #determine if parameter set is accepted or rejected
  error = matrix(0, n.time, D)
  for (d in 1:D) { #for each data type
    error[,d] = (data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[,d+1]
    error[,d] = (error[,d]*var(error[,d]))/var.jbest[d] #variance normalization
    
    j[d] = sum((error[,d]^2))/n.time #calculate uncertainty weighted error term after variance normalizaiton
    
    #chi-square test
    accept = rep (0, D)
    chi = rep(0, D)
    chi[d] = (j[d] - 1)^2
    if((1 - pchisq(chi[d], df[d])) < 0.1) {
      accept[d] = 1}
  } #end of data type loop
    
  d.accept = sum(accept) #calculate the number of j's accepted
  
  if(d.accept==D) { #if all j's accepted
    param.keep = rbind(param.keep, parms) #keep parameter set
    num.accepted = num.accepted + 1 #add to number of parameter sets accepted
  }
    
      
  if(num.accepted == 10) {
    break
  } #end of if statement
} #end of repeat




