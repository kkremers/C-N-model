####Install package for sounds (alerts you when script is done running)
#install.packages("beepr")   #run this line of code only if the package hasn't been installed yet
#require(beepr)
#beep(5) #test sounds; there are 9 options; don't need to run this every time, just to choose a sound

######Synthetic data experiments######

#Get data ready
head(out) #this is the output from the model run
data.assim = out[,c(1:7)] #choose columns that you want
head(data.assim) #preview table

#####remove some data points for Stock data
time.keepSTOCK  = seq(100, length(time), 182) #keep data for once per year (~once per year)
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
#SOM_C
SOMC.assim = data.assim$SOM_C[match(time.keepSTOCK, data.assim$time)]  #create vector of data for only those timesteps
SOMC.assim = data.frame(time=time.keepSTOCK, SOMC.assim) #create a dataframe of the new data and the corresponding timesteps
head(SOMC.assim) #check table
data.assim$SOM_C=SOMC.assim$SOMC.assim[match(data.assim$time, SOMC.assim$time)] #change the data in the assimilation table to NAs
#SOM_N
SOMN.assim = data.assim$SOM_N[match(time.keepSTOCK, data.assim$time)]  #create vector of data for only those timesteps
SOMN.assim = data.frame(time=time.keepSTOCK, SOMN.assim) #create a dataframe of the new data and the corresponding timesteps
head(SOMN.assim)
data.assim$SOM_N=SOMN.assim$SOMN.assim[match(data.assim$time, SOMN.assim$time)] #change the data in the assimilation table to NAs
#Available_N
AvailN.assim = data.assim$Available_N[match(time.keepSTOCK, data.assim$time)]  #create vector of data for only those timesteps
AvailN.assim = data.frame(time=time.keepSTOCK, AvailN.assim) #create a dataframe of the new data and the corresponding timesteps
head(AvailN.assim)
data.assim$Available_N=AvailN.assim$AvailN.assim[match(data.assim$time, AvailN.assim$time)] #change the data in the assimilation table to NAs

#####remove some data points for flux data
time.keepFLUX = data$time[which(data$DOY<=250 & data$DOY>=150)] #keep data for summer only
#GPP
#GPP.assim = data.assim$GPP[match(time.keepFLUX, data.assim$time)]  #create vector of data for only those timesteps
#GPP.assim = data.frame(time=time.keepFLUX, GPP.assim) #create a dataframe of the new data and the corresponding timesteps
#head(GPP.assim) #check table
#data.assim$GPP=GPP.assim$GPP.assim[match(data.assim$time, GPP.assim$time)] #change the data in the assimilation table to NAs
#NEE
NEE.assim = data.assim$NEE[match(time.keepFLUX, data.assim$time)]  #create vector of data for only those timesteps
NEE.assim = data.frame(time=time.keepFLUX, NEE.assim) #create a dataframe of the new data and the corresponding timesteps
head(NEE.assim) #check table
data.assim$NEE=NEE.assim$NEE.assim[match(data.assim$time, NEE.assim$time)] #change the data in the assimilation table to NAs
#Re
#Re.assim = data.assim$Re[match(time.keepFLUX, data.assim$time)]  #create vector of data for only those timesteps
#Re.assim = data.frame(time=time.keepFLUX, Re.assim) #create a dataframe of the new data and the corresponding timesteps
#head(Re.assim) #check table
#data.assim$Re=Re.assim$Re.assim[match(data.assim$time, Re.assim$time)] #change the data in the assimilation table to NAs
#NDVI
#NDVI.assim = data.assim$NDVI[match(time.keepFLUX, data.assim$time)]  #create vector of data for only those timesteps
#NDVI.assim = data.frame(time=time.keepFLUX, NDVI.assim) #create a dataframe of the new data and the corresponding timesteps
#head(NDVI.assim) #check table
#data.assim$NDVI=NDVI.assim$NDVI.assim[match(data.assim$time, NDVI.assim$time)] #change the data in the assimilation table to NAs
head(data.assim) #preview

#plot to view data
par(mfrow=c(3,2), mar=c(4,4,4,4))
plot(data.assim$Biomass_C~data.assim$time, pch=16, ylab="Biomass_C", xlab="Time (days)", ylim=c(300, 500))
plot(data.assim$Biomass_N~data.assim$time, pch=16, ylab="Biomass_N", xlab="Time (days)", ylim=c(4.4, 4.8))
plot(data.assim$SOM_C~data.assim$time, pch=16, ylab="SOM_C", xlab="Time (days)", ylim=c(1400, 2000))
plot(data.assim$SOM_N~data.assim$time, pch=16, ylab="SOM_N", xlab="Time (days)", ylim=c(30, 40))
plot(data.assim$Available_N~data.assim$time, pch=16, ylab="Available_N", xlab="Time (days)", ylim=c(0, 0.15))
plot(data.assim$NEE~data.assim$time, pch=16, ylab="NEE", xlab="Time (days)")


head(data.assim)
data.compare1 = data.assim[,c(1:3,7)] #pull out columns for data that you want to assimilate
sigma.obs1 = data.frame(matrix(NA, length(data.compare1$time), length(data.compare1))) #observation errors for each data type 
sigma.obs1[,1] = data.assim$time
sigma.obs1[!is.na(data.compare1[,2]),2] = 52
sigma.obs1[!is.na(data.compare1[,3]),3] = 0.84
sigma.obs1[!is.na(data.compare1[,4]),4] = 3649
sigma.obs1[!is.na(data.compare1[,5]),5] = 121
sigma.obs1[!is.na(data.compare1[,6]),6] = 0.13
sigma.obs1[!is.na(data.compare1[,7]),7] = 1


colnames(sigma.obs1) = colnames(data.compare1)
#sigma.obs1: columns need to be in SAME ORDER as data.compare1
head(data.compare1)
head(sigma.obs1)


###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = 9 #number of parameters to estimate
M = 100000 #number of iterations
D = length(data.compare1)-1 #number of data types being assimilated (number of columns in data.compare1, minus the "time" column)
n.time = rep(1, D) #create a vector to store the number of timepoints with data for each data stream
for(d in 1:D) { #for each data type
  n.time[d]=sum(!is.na(data.compare1[,d+1])) #calculate the number of time points that DO NOT have NA's
} #end of for loop
n.time #check 


#set up vectors with min and max values for each parameter (basically, using a uniform distribution as your "prior")
param.max=c(3,0.0024,0.85,0.98,0.012,0.9,0.9,3.3,0.00082)
param.min=c(1,0.0001,0.6,0.26,0.002,0.1,0.1,1.4,0.0000014)

#storage matrices
J = rep(1E100, M) #storage vector for cost function output
j=matrix(1E100, M, D) #to store error calculations for this iteration
all.draws = data.frame(matrix(1, M, n.param)) #storage for all parameter estimate iterations;
colnames(all.draws) = c(names(params))
param.est = data.frame(matrix(1, M, n.param)) #storage for accepted parameter estimate iterations;
param.est[1,]=(param.max-param.min)/2 #change first row to the value in the middle of the range
all.draws[1,]=(param.max-param.min)/2 #change first row to the value in the middle of the range
colnames(param.est) = c(names(params))
head(param.est) #check to make sure this is correct
head(all.draws)


#starting values for states
state <- c(Biomass_C = 400, 
           Biomass_N = 4.5, 
           SOM_C = 1600, 
           SOM_N = 35,
           Available_N = 0.1)

#set initial values
anneal.temp0=20000 #starting temperature
anneal.temp=20000 #starting temperature
iter=1 #simulated annealing iteration counter
reject=0 #reset reject counter


#start exploration

for (i in 2:M) {
  
  repeat{
    for(p in 1:n.param){ #for each parameter
      param.est[i,p] = runif(1, param.min[p], param.max[p])
      all.draws[i,p] = param.est[i,p]
    } #end of parameter loop
    if(param.est[i,p]>param.min[p] & param.est[i,p]<param.max[p]){
      break
    } #end of if loop
  } #end of repeat

    parms = as.numeric(param.est[i,]) #parameters for model run
    names(parms) = names(params) #fix names
    out = data.frame(solvemodel(parms, state)) #run model  
  
    if(any(is.na(out)) | any(out[,2:6]<0)){ #if there are any NAs or negative stocks in the output
      reject = reject+1 #reject parameter set
      param.est[i,] = param.est[i-1,] #set current parameter set to previous parameter set
      J[i] = J[i-1] #set current J to previous J (the minimum J so far)
    } else { #if there are no NAs or negative stocks
  
  #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare

  out.compare1 = out[match(data.compare1$time, out$time),c(1:3,7)] #these columns need to match the ones that were pulled out before
  
  error.time=matrix(0, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
  for (d in 1:D) { #for each data type
    for (m in 1:length(data.compare1$time)){ #for each timestep
    if(!is.na(data.compare1[m,d+1])){ #if there is data at that timestep for that data stream
      error.time[m,d]=((data.compare1[m,d+1] - out.compare1[m,d+1])/sigma.obs1[m,d+1])^2 #calculates the error at that timestep for that data stream
      } #end of if statement
      #if there was no data at that timestep, the error will remain "0" so that it will not impact the sum calculation in the next step
    } #end of time step loop
    
    j[i,d] = sum(error.time[,d])/n.time[d] #calculate cost function for each data stream
    
  } #end of data type loop
  
  J[i] = sum(j[i,])/D #calculate aggregate cost function
  
  
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

  anneal.temp=anneal.temp0-(1*iter) #decrease temperature
  
  
  iter=iter+1 #increase number of iterations counter
  
  if(anneal.temp<500){ #if temperature drops to less than 100
    anneal.temp0=(9/10)*anneal.temp0 #change initial temp to 9/10 of previous initial
    anneal.temp=anneal.temp0 #jump back up to that temp
    iter=1 #reset iteration counter
  }
    
} #end of exploration

#beep(5)
#make plots to check for mixing and make sure parameter space is thuroughly explored
plot(all.draws[,1])
lines(param.est[,1], col="red", lwd="2")

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

save.image(file="Step1_NEE_BiomassCN_kplant13.Rdata")

