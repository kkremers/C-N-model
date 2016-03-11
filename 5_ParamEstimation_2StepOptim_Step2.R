#load packages
require(deSolve)

#######STEP 2: ESTIMATE PARAMETER UNCERTAINTY

#need to calculate the variance of the errors for the minimum j's
head(data.assim)
head(data.sigma)

#run model spin up for current parameter set
time = seq(1:length(DOY.spin))
Temp.d1 <- approxfun(x=time, y=Temp.spin, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin1, method="linear", rule=2)
scalseason.d1 <- approxfun(x=time, y=scal.seas.spin1, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)


#OPEN 3_Model.R and run it the first time
out.spin= data.frame(solvemodel(param.best, state)) #creates table of model output
plot(out.spin$Biomass_C)
end.time = length(out.spin[,1])
#adjust starting values
state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
            Biomass_N = out.spin$Biomass_N[end.time], 
            SOM_C = out.spin$SOM_C[end.time], 
            SOM_N = out.spin$SOM_N[end.time],
            Available_N = out.spin$Available_N[end.time])


time = seq(1:length(data$time))
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)

out= data.frame(solvemodel(param.best, state)) #creates table of model output
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,10]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,10]))
#pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
out.compare1 = out1[match(data.compare1$time, out1$time),c(1,7,11)] #these columns need to match the ones that were pulled out before
head(out.compare1)
head(data.compare1)
head(sigma.obs1)

#create storage matrices for error and variance
n.param = length(param.best) #number of parameters to estimate
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
n.par = n.param #number of parameters predicted by each data stream
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
      param.est[p] = param.best[p] + rnorm(1, 0, t*(param.max[p]-param.min[p]))
      parms = as.numeric(param.est) #parameters for model run
      names(parms) = names(params) #fix names
    } #end of parameter loop
    if(all(param.est>=param.min) & all(param.est<=param.max)){
      
      #RUN MODEL SPINUP
      time = seq(1:length(DOY.spin))
      Temp.d1 <- approxfun(x=time, y=Temp.spin, method="linear", rule=2)
      TempAvg.d1 <- approxfun(x=time, y=TempAvg.spin, method="linear", rule=2)
      PAR.d1 <- approxfun(x=time, y=PAR.spin, method="linear", rule=2)
      scaltemp.d1 <- approxfun(x=time, y=scal.temp.spin1, method="linear", rule=2)
      scalseason.d1 <- approxfun(x=time, y=scal.seas.spin1, method="linear", rule=2)
      DOY.d1 <- approxfun(x=time, y=DOY.spin, method="linear", rule=2)
      Year.d1 <- approxfun(x=time, y=Year.spin, method="linear", rule=2)
      
      state  <- c(Biomass_C = 685, 
                  Biomass_N = 12.5, 
                  SOM_C = 19250, 
                  SOM_N = 850,
                  Available_N = 1.75)
      
      out.spin= data.frame(solvemodel(parms, state)) #creates table of model output
      #adjust starting values
      state <- c( Biomass_C = out.spin$Biomass_C[end.time], 
                  Biomass_N = out.spin$Biomass_N[end.time], 
                  SOM_C = out.spin$SOM_C[end.time], 
                  SOM_N = out.spin$SOM_N[end.time],
                  Available_N = out.spin$Available_N[end.time])
      
      if(all(!is.na(state))){   # & all(state>=state.min)
        break
      } #end of if loop
    } #end of if loop
  } #end of repeat
  
  time = seq(1:length(data$time))
  Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=data$time, y=data$Temp_avg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=data$time, y=data$PAR_ARF, method="linear", rule=2)
  scaltemp.d1 <- approxfun(x=data$time, y=scal.temp.sm, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=data$time, y=scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=data$time, y=data$year, method="linear", rule=2)
  
  out = data.frame(solvemodel(parms, state)) #run model  
  
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

save.image(file="Step2_NEE_NDVI_031016.Rdata")
