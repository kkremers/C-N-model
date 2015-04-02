####Install package for sounds (alerts you when script is done running)
#install.packages("beepr")   #run this line of code only if the package hasn't been installed yet
#require(beepr)
#beep(5) #test sounds; there are 9 options; don't need to run this every time, just to choose a sound

######Synthetic data experiments######

#Get data ready
head(out) #this is the output from the model run
data.assim = out[,c(1:8, 10,11)] #choose columns that you want
head(data.assim) #preview table

#add some noise to the data
for (i in 1:length(data.assim[,1])){
  for (j in 2:length(data.assim)){
    data.assim[i,j] = data.assim[i,j] + rnorm(1, 0, abs((min(data.assim[,j])/1000)))
    
  } 
}
head(data.assim) #check output


#####remove some data points from LAI data
time.keepLAI  = seq(1, length(time), 15) #keep LAI data for every 15 days
LAI.assim = data.assim$LAI[match(time.keepLAI, data.assim$time)]  #create vector of LAI data for only those timesteps
LAI.assim = data.frame(time=time.keepLAI, LAI.assim) #create a dataframe of the new LAI data and the corresponding timesteps
head(LAI.assim) #check table
data.assim$LAI=LAI.assim$LAI.assim[match(data.assim$time, LAI.assim$time)] #change the LAI data in the assimilation table to NAs
head(data.assim) #preview
data.assim$LAI #check

#####remove some data points for Stock data
time.keepSTOCK  = seq(200, length(time), 365) #keep data for once per year (~twice per year)
#Biomass_C
BiomassC.assim = data.assim$Biomass_C[match(time.keepSTOCK, data.assim$time)]  #create vector of data for only those timesteps
BiomassC.assim = data.frame(time=time.keepSTOCK, BiomassC.assim) #create a dataframe of the new data and the corresponding timesteps
head(BiomassC.assim) #check table
data.assim$Biomass_C=BiomassC.assim$BiomassC.assim[match(data.assim$time, BiomassC.assim$time)] #change the data in the assimilation table to NAs
#Biomass_N
BiomassN.assim = data.assim$Biomass_N[match(time.keepSTOCK, data.assim$time)]  #create vector of data for only those timesteps
BiomassN.assim = data.frame(time=time.keepSTOCK, BiomassN.assim) #create a dataframe of the new data and the corresponding timesteps
head(BiomassN.assim)
data.assim$Biomass_N=BiomassN.assim$BiomassN.assim[match(data.assim$time, BiomassN.assim$time)] #change the data in the assimilation table to NAs
#Available_N
AvailN.assim = data.assim$Available_N[match(time.keepSTOCK, data.assim$time)]  #create vector of data for only those timesteps
AvailN.assim = data.frame(time=time.keepSTOCK, AvailN.assim) #create a dataframe of the new data and the corresponding timesteps
head(AvailN.assim)
data.assim$Available_N=AvailN.assim$AvailN.assim[match(data.assim$time, AvailN.assim$time)] #change the data in the assimilation table to NAs
head(data.assim) #preview

#plot to view data
par(mfrow=c(3,2))
head(data.assim)
plot(data.assim$Biomass_C~data.assim$time, pch=16, ylab="Biomass_C", xlab="Time (days)")
plot(data.assim$Biomass_N~data.assim$time, pch=16, ylab="Biomass_N", xlab="Time (days)")
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


#save workspace - need this to run optimizaiton using CRC Super Computer
save.image(file="Workspace040115.Rdata")

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
#t = 0.5  # t is used to adjust the step size to keep acceptance rate at 50 % +/- 2.5% -- helps with mixing
anneal.temp0=12000 #starting temperature
anneal.temp=12000 #starting temperature
iter=1 #simulated annealing iteration counter
reject=0 #reset reject counter

#start exploration
for (i in 2:M) { #for each iteration
  
  #draw a parameter set from proposal distribution
  for(p in 1:n.param){ #for each parameter
    #repeat { #repeat until proposed parameter is within specified range
      #step.size = t*(param.max[p]-param.min[p]) #step size is a fraction of the inital parameter range
      param.est[i,p] = runif(1, param.min[p], param.max[p]) #param.est[i-1,p] +  rnorm(1, 0, step.size) #draw new parameter set
      #if(param.est[i,p]>param.min[p] && param.est[i,p]<param.max[p]){ #if the proposed parameter is in the specified range
       # break #break the repeat loop
      #}#end of if loop
    #} #end of repreat loop
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
  
  #tnew = NULL
  
  diff=J[i]-J[i-1] #calculate probability that proposed parameter is accepted
 
  
  if(diff>0){ #if difference is > 0 (or if the current J is greater than the previous J)
    
    u=runif(1, 0, 1) #draw random number between 0 and 1
    prob=exp((-1*diff)/anneal.temp) #simulated annealing
    
    if(u>=prob){    
    reject = reject+1 #reject parameter set
    param.est[i,] = param.est[i-1,] #set current parameter set to previous one
    J[i] = J[i-1] #set current J to previous J (the minimum J so far) - This makes it easier to find the minimum J at the end of the MCMC - it will always be the last value
    anneal.temp=anneal.temp0-(1*iter) #decrease temperature
    #tnew = 0.9*t #decrease the size of the parameter space
    } #else { #if u<prob (accept)
      #tnew=1.1*t #increase t 
    #} 
  }
  
  #if (diff<=0) {#accept all of these because current J is smaller than previous J
  #  tnew=1.1*t #increase t
  #} #end of if diff
  
  acceptance = 1 - (reject / i) #calculate proportion of accepted iterations
  
  #If the acceptance rate is not 20% +/- 2.5%, then adjust "t"
  #if(acceptance > 0.275) {
  #  t = tnew
  #} 
  #if (acceptance < 0.225) {
  #  t = tnew
  #}
  
  iter=iter+1 #increase number of iterations counter
  
  if(anneal.temp<100){ #if temperature drops to less than 100
    anneal.temp0=(9/10)*anneal.temp0 #change initial temp to 9/10 of previous initial
    anneal.temp=anneal.temp0 #jump back up to that temp
    iter=1 #reset iteration counter
  }
  
  
} #end of exploration

#beep(5)

plot(param.est[,1], type="l") #make plots to check for mixing


#The final iteration should be the smallest J
steps=seq(1:length(J)) #create a vector that represents the number of steps or iterations run
J=data.frame(steps, J) #create a dataframe that has "steps" as the first column and "J" as the second column
head(J); tail(J) #check the table
step.best = J[which.min(J[,2]),1] #determine which step has the minimum value of J and store as "step.best"
param.est[step.best,] #show the parameter set that resulted in the best J
param.best = as.numeric(param.est[step.best,]) #store that parameter set as param.best
names(param.best) = names(params) #change the names to match params
j.best = j[step.best,] #pull out the minimum j
param.best #view the best parameter set
j.best #view the minimum J


param.step1 = param.est #storing the iterations under a different name in case you need them later


#######STEP 2: ESTIMATE PARAMETER UNCERTAINTY

#need to calculate the variance of the errors for the minimum j's

out = data.frame(solvemodel(param.best, state)) #run model
#pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
out.compare1 = out[match(data.compare1$time, out$time),c(1,10,11)] #these columns need to match the ones that were pulled out before
head(out.compare1)
head(data.compare1)


#create storage matrices for error and variance
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
param.keep = data.frame(matrix(1, 5, n.param)) #storage for parameter estimate iterations; 
colnames(param.keep) = c(names(param.best))
head(param.keep)#check to make sure this is correct


#set initial values
param.est = param.best #set initial values for parameters
t = t #keep the step size that was used in previous step
reject=0 #reset reject counter
num.accepted = 0 #counter for number of accepted parameters - when this gets to 1000, loop will stop
num.reps = 0 #counter for number of repititions - calculates acceptance rate

#also need to know degrees of freedom for chi square test
df = rep(0, D)
for (d in 1:D) { #for each data type
  df[d] = n.time[d] - n.param
} #end of data loop
df #check values

#start loop (8:20AM, 03/31/15)

repeat { #repeat until desired number of parameter sets are accepted
    
    num.reps=num.reps+1 #add to number of reps counter
      
    #draw a parameter set from proposal distribution
    for(p in 1:n.param){ #for each parameter
      repeat { #repeat until proposed parameter is within specified range
        step.size = t*(param.max[p]-param.min[p]) #step size is a fraction of the inital parameter range
        param.est[p] = param.est[p] +  rnorm(1, 0, step.size) #draw new parameter set
        if(param.est[p]>param.min[p] && param.est[p]<param.max[p]){ #if the proposed parameter is in the specified range
          break #break the repeat loop
        }#end of if loop
      } #end of repreat loop
    } #end of parameter loop


  #run model and calculate error function 
  parms = as.numeric(param.est) #parameters for model run
  names(parms) = names(params) #fix names
  out = data.frame(solvemodel(parms, state)) #run model
  #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
  out.compare1 = out[match(data.compare1$time, out$time),c(1,10,11)] #these columns need to match the ones that were pulled out before
  #remove the time column - no longer needed
  data.comp = data.compare1[,-1]
  out.comp = out.compare1[,-1]
  sigma = sigma.obs1[,-1]

  #determine if parameter set is accepted or rejected
  error = matrix(NA, length(data.comp[,1]), D)
  var.error=rep(0,D)  
  for (d in 1:D) { #for each data type
    for (m in 1:length(data.comp[,1])){ #for each timestep
      if(!is.na(data.comp[m,d])){ #if there is data at that timestep for that data stream
        error[m,d]=((data.comp[m,d] - out.comp[m,d])/sigma[m,d])^2 #calculates the error at that timestep for that data stream
      } #end of if statement
    } #end of time step loop
    var.error[d] = var(error[!is.na(data.comp[,d]),d]) #calculate variance of the errors (excludes NAs)
  } #end of data type loop
  
  for (d in 1:D) { #for each data type
    for (m in 1:length(data.comp[,1])){ #for each timestep
        error[m,d] = (error[m,d]*sqrt(var.jbest[d]))/sqrt(var.error[d]) #variance normalization
    } #end of time step loop  
    
    j[d] = sum(error[!is.na(data.comp[,d]),d]^2)/n.time[d] #calculate uncertainty weighted error term after variance normalizaiton
  } #end of data type loop
  
    #chi-square test
    accept = rep (0, D) #vector to keep track of if each j has been accepted or rejected; 1=accept, 0=reject
    chi = rep(0, D) #to store chi values for each data type
  for (d in 1:D) { #for each data type  
    chi[d] = (j[d] - 1)^2 #calcualte chi squared value for each data type
    if (is.na(chi[d])) { #if a chi value is NA
      chi[d] = 0 #set to 0
    }
    if(pchisq(chi[d], df[d]) < 0.9) { #conduct chi square test
      accept[d] = 1} #if accepted, change value in accept vector to 1
  } #end of data type loop
    
  d.accept = sum(accept) #calculate the number of j's accepted
  
  if(d.accept==D) { #if all j's are accepted
    num.accepted = num.accepted+1 #add to number of parameter sets accepted
    tnew=1.1*t #increase the step size
    param.keep[num.accepted,]=param.est #store the parameter set in the storage dataframe
  }
  if(d.accept<D) { #if any j's rejected
    reject = reject+1 #reject parameter set
    tnew=0.9*t #decrease the step size
  }
    
  acceptance = 1 - (reject / num.reps) #calculate proportion of accepted iterations

  #If the acceptance rate is not 20% +/- 2.5%, then adjust "t"
  if(acceptance > 0.275) {
    t = tnew
  } 
  if (acceptance < 0.225) {
    t = tnew
  }

  if (num.accepted==1000) { #if you have accepted the number of parameter sets you want (i.e., 1000)
    break  #break repeat loop
  } 

} #end of repeat

#beep(5)

