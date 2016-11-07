#load packages
require(deSolve)

#######STEP 2: ESTIMATE PARAMETER UNCERTAINTY

#need to calculate the variance of the errors for the minimum j's
head(data.assim)
head(data.sigma)


#replace 1st row with values for current parameters
out.compare1=NULL
for(i in 1:length(latitudes)){
  lat.i = latitudes[i]  
  dat.i = subset(data.ALL, Latitude==lat.i)
  data.compare.i = subset(data.compare1, Latitude==lat.i)
  time=seq(1:length(dat.i[,1]))
  
  #make into functions so that it will be continuous in the model
  Temp.d1 <- approxfun(x=time, y=dat.i$Temp, method="linear", rule=2)
  TempAvg.d1 <- approxfun(x=time, y=dat.i$Tavg, method="linear", rule=2)
  PAR.d1 <- approxfun(x=time, y=dat.i$PAR, method="linear", rule=2)
  scalseason.d1 <- approxfun(x=time, y=dat.i$scal.seas, method="linear", rule=2)
  DOY.d1 <- approxfun(x=time, y=dat.i$DOY, method="linear", rule=2)
  Year.d1 <- approxfun(x=time, y=dat.i$Year, method="linear", rule=2)
  
  state.i = subset(state.dat, Latitude==lat.i)
  state.i = as.numeric(state.i[3:7])
  names(state.i)=names(state) 
  
  out= data.frame(solvemodel(param.best, state.i)) #creates table of model output
  out1 = out[match(data.compare.i$time, out$time),c(1,11)] #these columns need to match the ones that were pulled out before
  lat = rep(lat.i, length(out1[,1]))
  out1 = cbind(Latitude = lat, out1)
  
  out.compare1 = rbind(out.compare1, out1)
}  
head(out.compare1)
tail(out.compare1)
head(data.compare1)
tail(data.compare1)
head(sigma.obs1)
tail(sigma.obs1)

#create storage matrices for error and variance
n.param = length(param.best) #number of parameters to estimate
D = 1 #number of data types being assimilated 
n.time #check 

var.jbest = rep(0, D)
error.jbest=matrix(NA, length(data.compare1$time), D) #create data frame to store error calculations; want all to be "0" originally because if there is no data it will remain 0
for (d in 1:D) { #for each data type
  for (m in 1:length(data.compare1$time)){ #for each timestep
    if(!is.na(data.compare1[m,d+2])){ #if there is data at that timestep for that data stream
      error.jbest[m,d]=((data.compare1[m,d+2] - out.compare1[m,d+2])/sigma.obs1[m,d+2])^2 #calculates the error at that timestep for that data stream
    } #end of if statement
  } #end of time step loop
  
  var.jbest[d] = var(error.jbest[!is.na(data.compare1[,d+2]),d]) #calculate variance of the errors (excludes NAs)
  
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
state
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
      break
    } #end of if loop
  } #end of repeat
  
  out.compare1=NULL
  for(k in 1:length(latitudes)){
    lat.i = latitudes[k]  
    dat.i = subset(data.ALL, Latitude==lat.i)
    data.compare.i = subset(data.compare1, Latitude==lat.i)
    time=seq(1:length(dat.i[,1]))
    
    #make into functions so that it will be continuous in the model
    Temp.d1 <- approxfun(x=time, y=dat.i$Temp, method="linear", rule=2)
    TempAvg.d1 <- approxfun(x=time, y=dat.i$Tavg, method="linear", rule=2)
    PAR.d1 <- approxfun(x=time, y=dat.i$PAR, method="linear", rule=2)
    scalseason.d1 <- approxfun(x=time, y=dat.i$scal.seas, method="linear", rule=2)
    DOY.d1 <- approxfun(x=time, y=dat.i$DOY, method="linear", rule=2)
    Year.d1 <- approxfun(x=time, y=dat.i$Year, method="linear", rule=2)
    
    state.i = subset(state.dat, Latitude==lat.i)
    state.i = as.numeric(state.i[3:7])
    names(state.i)=names(state) 
    
    out= data.frame(solvemodel(parms, state.i)) #creates table of model output
    out1 = out[match(data.compare.i$time, out$time),c(1,11)] #these columns need to match the ones that were pulled out before
    lat = rep(lat.i, length(out1[,1]))
    out1 = cbind(Latitude = lat, out1)
    
    out.compare1 = rbind(out.compare1, out1)
  } #end of latitudes loop   
  
    #remove the time and latitude column - no longer needed
    data.comp = matrix(data.compare1[,-c(1,2)])
    out.comp = matrix(out.compare1[,-c(1,2)])
    sigma = matrix(sigma.obs1[,-c(1,2)])
    
    #determine if parameter set is accepted or rejected
    error = matrix(NA, length(data.comp[,d]), D)
    var.error=rep(0,D)  
    error=((data.comp - out.comp)/sigma)^2 #calculates the error at that timestep for that data stream
    
    for (d in 1:D) { #for each data type
      
      var.error[d] = var(error[!is.na(data.comp),d]) #calculate variance of the errors (excludes NAs)
      
      for (m in 1:length(data.comp[,1])){ #for each timestep
        error[m,d] = (error[m,d]*sqrt(var.jbest[d]))/sqrt(var.error[d]) #variance normalization
      } #end of time step loop  
      
      j[d] = sum(error[!is.na(data.comp),d]) #calculate cost function for each data stream after variance normalizaiton
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

save.image(file="Step2_Alpha_092916.Rdata")
plot(density(param.keep$ALPHA))
