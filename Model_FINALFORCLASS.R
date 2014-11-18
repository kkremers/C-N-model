####Load packages
require(deSolve)
require(FME)


#
####################################

#using real data

#first upload and viewdata 

data = read.csv("OriginalData_NOTfilled.csv") #choose file from directory
names(data) = c("time", "year", "DOY", "Albedo", "Temp_T", "PAR_T", "Temp_ARF", "PAR_ARF", "EVI", "NDVI", "LAI", "NEE", "Re", "GPP")
head(data) #view table

#plot data
par(mfrow=c(2,1)) 
plot(data$Temp_T~data$time, type="l", ylab = "Daily Max Temp (C)", xlab="Time (days)") 
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16)
plot(data$PAR_T~data$time, type="l", ylab = "Daily Max PAR (umol m-2 s-1)", xlab="Time (days)")
points(data$PAR_ARF~data$time, col="blue", pch=16)
#ARF data is missing winter measurements - want to fill this in

#want to plot to determine relationship, and then fill in missing data in ARF data
#start with temperature
linmod.t = lm(data$Temp_ARF~data$Temp_T + 0) #linear model, set intercept = 0
summary(linmod.t) #summary
slope.t = summary(linmod.t)$coefficients[1,1] #slope value

par(mfrow=c(1,2))
plot(data$Temp_ARF~data$Temp_T, ylab = "ARF", xlab="Toolik", main="Temperature", pch=16, col="red")
abline(linmod.t, lwd = 2) #linear regression line

#do the same thing for PAR data
linmod.p = lm(data$PAR_ARF~data$PAR_T + 0)
summary(linmod.p)
slope.p = summary(linmod.p)$coefficients[1,1]

par(mfrow=c(1,1))
plot(data$PAR_ARF~data$PAR_T, ylab = "ARF", xlab="Toolik", main="PAR", pch=16, col="blue")
abline(linmod.p, lwd = 2)

#fill in missing data - NEED TO FIGURE OUT HOW TO CARRY UNCERTAINTY THROUGH THIS???

for(i in 1:length(data$Temp_ARF)){
  
  if(is.na(data$Temp_ARF[i])==TRUE) {
    data$Temp_ARF[i] = data$Temp_T[i]*slope.t 
  }
  
  if(is.na(data$PAR_ARF[i])==TRUE) {
    data$PAR_ARF[i] = data$PAR_T[i]*slope.p
  }
}

#need to convert units of PAR
data$PAR_ARF = data$PAR_ARF*(1E-6)*86400
data$PAR_T = data$PAR_T*(1E-6)*86400

#check output to make sure it all lines up
par(mfrow=c(2,1))
plot(data$Temp_T~data$time, type="l", ylab = "Daily Max Temp (C)", xlab="Time (days)")
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16)
plot(data$PAR_T~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", xlab="Time (days)")
points(data$PAR_ARF~data$time, col="blue", pch=16)

#Now we want to filter so that we only use the PAR that the plants are exposed to
#To do this, we will use albedo to create a new vector, PAR_vis, that only includes PAR data for snow-free days

PAR_vis = NULL

for(i in 1:length(data$PAR_ARF)){
  
  if(data$Albedo[i] > 0.2 ) { #if albedo is greater than 0.2
    PAR_vis[i] = 0
  }
  else {
    PAR_vis[i] = data$PAR_ARF[i]
  }
  
}

data = data.frame(data[,1:8], PAR_vis = PAR_vis, data[,9:14])
head(data)


#check output
par(mfrow=c(2,1))
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", main="All PAR", xlab="Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", main="Plant Available PAR", xlab="Time (days)")


write.csv(data, "FluxData.csv") #added the updated data to the working directory so that it is easy to access - won't have to do any of the above steps again
##################################################

############re-loading the data

#set working directory to C-N-model
data = read.csv("InputData_Processed.csv")
head(data)

#plot the data
par(mfrow=c(2,2), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$delGDD~data$time, type="l", ylab = "delGDD (change in degrees C /day)",  xlab="", col="forestgreen")
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")



#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_vis, method="linear", rule=2)
delGDD.d1 <-approxfun(x=data$time, y=data$delGDD, method="linear", rule=2)


######################Parameters and state variables##########################
params <- c(LitterRate = 0.0012,
            DecompRateC = 0.00004, 
            DecompRateN = 0.00017,
            retrans = 0.9,  
            RespRateSOM = 1E-6, 
            RespRateL = 2.5E-3,
            kCUE = 0.001,
            CUEmax = 0.6,
            kplant = 0.11,
            UptakeMax = 0.01,
            Available_N = 0.1, 
            Biomass_C = 200, 
            Biomass_N = 3.5, 
            Litter_C = 90, 
            Litter_N = 2, 
            SOM_C = 2000, 
            SOM_N = 57)


####################MODEL#################################
time = seq(1, 1826, 1)

solvemodel <- function(params, times=time) {
  
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      PAR=PAR.d1(t)
      delGDD = delGDD.d1(t)
      
      delGDD.max = max(data$delGDD) 
      delGDD.min = min(data$delGDD) 
      #constants for PLIRTLE model - Loranty 2011
      k=0.63
      Pmax = 1.18
      E0 = 0.03
      q10 = 2
      LAC = 0.012
      
      
      #FLUXES
      s.GDD = (delGDD - delGDD.min)/(delGDD.max-delGDD.min)
      LAI = (Biomass_C*0.4)*LAC*s.GDD
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI ) ) ) * 12 
      Uptake =  UptakeMax * LAI * ( Available_N / ( kplant + Available_N ) )
      cue = CUEmax * (Uptake/(kCUE + Uptake))
      Rh1 =  RespRateL * Litter_C * ( q10 ^ ( Temp / 10 ) )
      Rh2 =  RespRateSOM * SOM_C * ( q10 ^ ( Temp / 10 ) )
      Decomp_C =  DecompRateC * Litter_C * ( q10 ^ ( Temp / 10 ) )
      Decomp_N =  DecompRateN* Litter_N * ( q10 ^ ( Temp / 10 ) )
      Litterfall_N =  LitterRate * Biomass_N * ( 1 - retrans )
      Litterfall_C =  LitterRate * Biomass_C
      Ra =  ( 1 - cue ) * GPP
      NetN =  0.00015 * ( q10 ^ (-Temp / 10 ) )
      N_dep = 0.00008
      
            
      
      #calculated variables to use for model fitting and analysis
      Re= Ra+Rh1+Rh2
      NEE = Re - GPP
      
      
      #differential equations
      dAvailable_N = NetN  + N_dep  - Uptake 
      dBiomass_C = GPP  - Ra  - Litterfall_C 
      dBiomass_N = Uptake  - Litterfall_N 
      dLitter_C = Litterfall_C  - Rh1  - Decomp_C 
      dLitter_N = Litterfall_N  - Decomp_N 
      dSOM_C = Decomp_C  - Rh2 
      dSOM_N = Decomp_N  - NetN
      
      
      #what to output
      
      list(c(dAvailable_N, 
             dBiomass_C, 
             dBiomass_N, 
             dLitter_C, 
             dLitter_N, 
             dSOM_C, 
             dSOM_N),
           c(GPP=GPP, LAI=LAI, NEE=NEE, Re=Re, 
             cue=cue, Ra=Ra, Rh1=Rh1, Rh2=Rh2, 
             Uptake = Uptake, s.GDD=s.GDD))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=params[11:17],times=time,func=model,parms = params[1:10], method="rk4")) #integrate
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params)) #creates table of model output

##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")



#plot pools
par(mfrow=c(3,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="springgreen3", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="springgreen3",  main = "Biomass N", xlab="", ylab="g N m-2", lty=2)
plot(out$Litter_C~out$time, type="l", col="orange", main = "Litter C", xlab="", ylab="g C m-2")
plot(out$Litter_N~out$time, type="l", col="orange", main = "Litter N", xlab="", ylab="g N m-2", lty=2)
plot(out$SOM_C~out$time, type="l", col="red", main = "SOM C", xlab="Time (days)", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="red", main = "SOM N", xlab="Time (days)", ylab="g N m-2",lty=2)
plot(out$Available_N~out$time, type="l", col="darkolivegreen3", main = "Available N", xlab="", ylab="g N m-2")



#see how well data matches
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$GPP~out$time, col="forestgreen", pch=18, ylim=c(0,6), main="GPP", ylab="Flux (gC m-2 day-1)", xlab="")
points(data$GPP, col="blue", pch=16, cex=0.6)

plot(out$LAI~out$time, col="orange", pch=18, ylim=c(0,1), main="LAI", ylab="LAI (m2 leaf m-2 ground)", xlab="" )
points(data$LAI, col="blue", pch=16, cex=0.6)

plot(-out$Re~out$time, col="red", pch=16, ylim=c(-5,0), main="Re", xlab="Time (days)", ylab="Flux (gC m-2 day-1)")
points(-data$Re, col="blue", pch=16, cex=0.6)
abline(h=0)

plot(out$NEE~out$time, col="olivedrab3", pch=18, ylim=c(-3,2), main="NEE", xlab="Time (days)", ylab="Flux (gC m-2 day-1)")
points(data$NEE, col="blue", pch=16, cex=0.6)
abline(h=0)


#plot CUE and LAI
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$cue~out$Uptake, xlab = "Uptake (g N m-2 day-1)", ylab = "Carbon Use Efficiency (CUE)")
plot(out$cue~out$time, type="l",  xlab = "Time (days)", ylab = "Carbon Use Efficiency (CUE)")
plot(out$Uptake~out$Available_N, xlab = "Available N (g N m-2)", ylab = "Uptake (g N m-2 day-1)")
plot(out$Uptake~out$time, type="l",  xlab = "Time (days)", ylab = "Uptake (g N m-2 day-1)")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$s.GDD~data$delGDD, xlab = "delGDD", ylab = "Scalar (s.GDD)")
plot(out$LAI~data$delGDD, xlab = "delGDD", ylab = "LAI (m2 m-2)")
plot(out$LAI~out$Biomass_C, xlab = "Biomass_C (gC m-2)", ylab = "LAI (m2 m-2)")


############SENSITIVITY ANALYSIS USING LME PACKAGE###############

sensvars = c("Biomass_C", 
             "Biomass_N", 
             "Litter_C", 
             "Litter_N", 
             "SOM_C", 
             "SOM_N",
             "Available_N",
             "GPP",
             "NEE",
             "Re",
             "LAI")


#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=params, senspar = names(params), 
                   sensvar = sensvars, varscale = 1)

head(s.local)
s.local.summ = summary(s.local, var=T)
s.loc.summ.ordered = s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] 
write.csv(s.loc.summ.ordered, "c:/Users/Rocha Lab/Desktop/Kelsey/LocalSensitivityAnalysis.csv") #univariate sensitivity
param.cor = data.frame(cor(s.local[,c(-1,-2)]))#table of parameter correlations
param.cor
write.csv(param.cor, "c:/Users/Rocha Lab/Desktop/Kelsey/ParamCorr.csv") #bivariate sensitivity
pairs(s.local)


#global sensitivity analysis

#alter all params by 50%
parms = as.vector(unlist(params))
paramsperc =parms*0.5
params.min =  parms - paramsperc
params.max = parms + paramsperc
parRanges = data.frame(min = params.min,  max = params.max)
rownames(parRanges) = names(params)
parRanges

s.global <- sensRange(func=solvemodel, parms=params, dist ="unif", 
                      sensvar = sensvars, parRange=parRanges, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "day", ylab = "g/m2", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


##############IDENTIFY PARAMS THAT CAN BE ESTIMATED###########

#this code is currently a little wonky - need to work on it

#load data to assimilate

data.assim = read.csv("ALLData_Assim.csv") #load data
head(data.assim)
data.assim = data.assim[,c(1,3:12)] #pull out columns that you need
head(data.assim)
#data.assim = data.assim[complete.cases(data.assim),] #only complete cases
#head(data.assim)
#create a function that returns the residuals of the model vs. the data (estimated by "modCost")
Objective <- function(x, parset=names(x)){ 
  params[parset] <- x
  out <- data.frame(solvemodel(params, state))
  return(modCost(obs = data.assim, model=out[match(data.assim$time, out$time),c(1, 11, 12, 9, 2:8)]))
}

#establish wich parameters can be estimated by the dataset
sF1 <- sensFun(func=Objective, parms=params[1:10], varscale=1) 
summary(sF1)
coll1 = collin(sF1)
plot(coll1, log="y")
abline(h=20, col="red") #if collinearity is less than 20, it is generally okay to estimate those parameters
head(coll1)
coll1[coll1$collinearity<20 & coll1$N ==7,]

<<<<<<< HEAD
=======
#######################ESTIMATE DATA UNCERTAINTY######################

#######################TEST MCMC###########################


#Get fake data ready
head(out)
data.assim = out[1:12]
head(data.assim)

#add noise to data
#for (j in 2:length(data.assim)) {
#  for (i in 2:length(time)){
#    data.assim[i,j]=data.assim[i,j]+ rnorm(1, 0, (sd(data.assim[,j])/4))
#      if(data.assim[i,j] < 0){
#        data.assim[i,j] = data.assim[i-1,j]
#      }
#    }
#  }
#set.seed(1) #to get same noise every time

#remove some data points
time.keep  = seq(1, length(time), 10) #keep data for every 10 days
data.assim = data.assim[match(time.keep, data.assim$time),] 
head(data.assim)

#plot to see what it looks like
plot(data.assim[,3])

data.compare1 = data.assim[1:8] #pull out columns that you need
sigma.obs1 = matrix(1, length(data.compare1$time), 8) #observation erros for each data type - data frame with 288 rows and 4 columns corresponding to each data type
sigma.obs1[,1] = data.assim$time
#FOR sigma.obs1: columns need to be in SAME ORDER as data.compare1
head(data.compare1)
head(sigma.obs1)

#plant/soil data
#data2 = read.csv("PlantAndSoilData_Assim.csv") #load plant and soil data
#data.compare2 = data2[,1:8] #pull out columns that you need
#sigma.obs2 = data2[,c(1,9:15)] #observation erros for each data type 
#FOR sigma.obs2: columns need to be in SAME ORDER as data.compare2
#head(data.compare2) #make sure in same order
#head(sigma.obs2)



###STEP 1: EXPLORE PARAMETER SPACE

#other necessary knowns
n.param = 17 #number of parameters
M = 1000 #number of iterations
D = 7 #number of data types being assimilated (4xflux, 7xplant/soil)

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
param.est[,8] = 0.05  #UptakeMax
param.est[,9] = 0.5  #kplant
param.est[,10] = 0.4 #CUEmax
param.est[,11] = 0.3 #Available_N
param.est[,12] = 250 #Biomass_C
param.est[,13] = 4 #Biomass_N
param.est[,14] = 150 #Litter_C
param.est[,15] = 3 #Litter_N
param.est[,16] = 3000 #SOM_C
param.est[,17] = 70 #SOM_N

head(param.est) #check to make sure this is correct


print(system.time( #prints the amount of time the MCMC took to run
  for (i in 2:M) { #for each iteration
    
    #draw a parameter set from prior distributions
    param.est[i,1] = runif(1, 0.0001, 0.1)  #LitterRate
    param.est[i,2] = runif(1, 0.000001, 0.01) #DecompRateC
    param.est[i,3] = runif(1, 0.00001, 0.01) #DecompRateN
    param.est[i,4] = runif(1, 0.1, 0.99) #retrans
    param.est[i,5] = runif(1, 0.0000001, 0.01) #RespRateSOM
    param.est[i,6] = runif(1, 0.0000001, 0.01)  #RespRateL
    param.est[i,7] = runif(1, 0.00001, 0.1)  #kCUE
    param.est[i,8] = runif(1, 0.1, 1)  #CUEMax
    param.est[i,9] = runif(1, 0.001, 1)  #kplant
    param.est[i,10] = runif(1, 0.001, 5) #Uptakemax
    param.est[i,11] = runif(1, 0.05, 5) #Available_N
    param.est[i,12] = runif(1, 100, 1000) #Biomass_C
    param.est[i,13] = runif(1, 3, 30) #Biomass_N
    param.est[i,14] = runif(1, 10, 500) #Litter_C
    param.est[i,15] = runif(1, 0.1, 5)  #Litter_N
    param.est[i,16] = runif(1, 1000, 5000) #SOM_C
    param.est[i,17] = runif(1, 20, 200) #SOM_N
    
    
    
    #run model and calculate error function 
    parms = as.numeric(param.est[i,]) #parameters for model run
    names(parms) = names(params) #fix names
    out = data.frame(solvemodel(parms)) #run model
    #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
    out.compare1 = out[match(data.compare1$time, out$time),c(1:12)] 
    #out.compare2 = out[match(data.compare2$time, out$time),1:8] 
    
    
    for (d in 1:D) { #for each data type
      
      j[i,d] = sum(((data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[,d+1])^2) #calculate uncertainty weighted error term
      
    } #end of data type loop
    
    J[i] = prod(j[i,]) #calculate aggregate cost function
    
  })) #end of exploration

  parms = param.est[which.min(J),] #new set of parameters is the set with the minimum cost function


###STEP 2: MCMC

#other necessary knowns
n.param = 17 #number of parameters
M = 1000 #number of iterations
D = 7 #number of data types being assimilated (4xflux, 7xplant/soil)
jump = parms*0.1

#storage matrices
param.est = data.frame(matrix(1, M, n.param)) #storage for parameter estimate iterations; 
colnames(param.est) = c(names(params)) #, names(state))
parm.est[1,] = parms #set starting values to the values chosen in step 1


reject = 0 #reset reject counter

for(i in 2:M){ #for each iteration
  for(j in n.param){ 
  parm.est[i,j] = rnorm(1, parm.est[i-1,], jump[j])} #draw parameter value from proposal distribution
  

  #calculate f(params), which is the target distribution
  #Compute ratio f(params proposed)/f(params previous)
  #draw u
  #accept or reject
  
  
  
  
  
}


>>>>>>> d11fbf299aa5ae03e8e3e9fbac44e0827e9af6a4

##############################MCMC using FME package####################
