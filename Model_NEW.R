####Load packages
require(deSolve)
require(FME)



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
data = read.csv("FluxData.csv")
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


######################Parameters##########################
params <- c(LitterRate = 0.0007,
            DecompRate = 0.00019, 
            retrans = 0.8,  
            RespRateSOM = 1E-6, 
            RespRateL = 5.5E-4,
            kplant = 0.08
            )



#######################STATE VARIABLES##################
state = c(Available_N = 0.1, 
          Biomass_C = 200, 
          Biomass_N = 3.75, 
          Litter_C = 200, 
          Litter_N = 3, 
          SOM_C = 2000, 
          SOM_N = 57)


####################MODEL#################################
time = seq(1, 1461, 1)

solvemodel <- function(params, state, times=time) {
  
  
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
      Uptake =  ((kplant*Available_N) / ( kplant + Available_N ))  * s.GDD
      cue = (0.5*Uptake)/((kplant/100)+Uptake)
      Litterfall_C =  LitterRate * Biomass_C
      Litterfall_N =  LitterRate * Biomass_N * ( 1 - retrans )
      Immobilization =   0.0013 * (q10 ^ (Temp / 70))
      Rh1 =  RespRateL * Litter_C * ( q10 ^ ( Temp / 10 ) )
      N_deposition =  0.0017
      Rh2 =  RespRateSOM * SOM_C * ( q10 ^ ( Temp / 10 ) )
      Decomp_N =  DecompRate* Litter_N * ( q10 ^ ( Temp / 10 ) )
      Decomp_C =  DecompRate* Litter_C * ( q10 ^ ( Temp / 10 ) ) 
      Ra =  ( 1 - cue ) * GPP
      Mineralization =  0.00007 * (q10 ^ (Temp / 10)) #schmidt et al. 1999
      
            
      #calculated variables to use for model fitting and analysis
      Re= Ra+Rh1+Rh2
      NEE = Re - GPP
      
      
      #differential equations
      dAvailable_N = Mineralization  + N_deposition  - Immobilization  - Uptake 
      dBiomass_C = GPP  - Ra  - Litterfall_C 
      dBiomass_N = Uptake  - Litterfall_N 
      dLitter_C = Litterfall_C  - Rh1  - Decomp_C 
      dLitter_N = Litterfall_N  - Decomp_N 
      dSOM_C = Decomp_C  - Rh2 
      dSOM_N = Decomp_N  + Immobilization  - Mineralization 
      
      
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
             Uptake = Uptake, s.GDD=s.GDD, Immobilization=Immobilization,
             Mineralization = Mineralization))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params, state)) #creates table of model output
#

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
plot(out$Available_N~out$time, type="l", col="darkolivegreen3", main = "Available N", xlab="", ylab="g C m-2")



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
plot(out$cue~out$Uptake, xlab = "Uptake", ylab = "Carbon Use Efficiency (CUE)")
plot(out$cue~out$time, type="l",  xlab = "Time (days)", ylab = "Carbon Use Efficiency (CUE)")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$s.GDD~data$delGDD, xlab = "delGDD", ylab = "Scalar (s.GDD)")
plot(out$LAI~data$delGDD, xlab = "delGDD", ylab = "LAI")


head(out)

plot(out$Uptake~out$time)

############SENSITIVITY ANALYSIS###############
paramsperc =  as.vector(unlist(params))*0.1
params.min =  params - paramsperc
params.max = params + paramsperc

parRanges = data.frame(min = params.min,  max = params.max)
rownames(parRanges) = names(params)
parRanges


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


#global sensitivity analysis

s.global <- sensRange(func=solvemodel, parms=params, state= state, dist ="unif", 
                      sensvar = sensvars, parRange=parRanges, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "day", ylab = "g/m2", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=params, state=state, senspar = names(params), 
                   sensvar = sensvars, varscale = 1)

head(s.local)
s.local.summ = summary(s.local, var=T)
s.loc.summ.ordered = s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] 
write.csv(s.loc.summ.ordered, "c:/Users/Rocha Lab/Desktop/Kelsey/LocalSensitivityAnalysis.csv")
param.cor = data.frame(cor(s.local[,c(-1,-2)]))#table of parameter correlations
write.csv(param.cor, "c:/Users/Rocha Lab/Desktop/Kelsey/ParamCorr.csv")
pairs(s.local)


##############IDENTIFY PARAMS THAT CAN BE ESTIMATED###########


#flux data was loaded in initial data table
head(data)
dat.assim=data[, c(1, 12:15)] #pull out columns that you want to assimilate, and the time column
colnames(dat.assim)=c("time", "LAI", "NEE", "Re", "GPP")
dat.assim = dat.assim[complete.cases(dat.assim),]
head(dat.assim)


#create a function that returns the residuals of the model vs. the data (estimated by "modCost")
Objective <- function(x, parset=names(x)){ 
  params[parset] <- x
  out <- data.frame(solvemodel(params, state))
  return(modCost(obs = dat.assim, model=out[match(dat.assim$time, out$time),c(1,10,11,12,9)]))
}

#establish wich parameters can be estimated by the dataset
sF1 <- sensFun(func=Objective, parms=params[-3], varscale=1) 
summary(sF1)
coll1 = collin(sF1)
plot(coll1, log="y")
abline(h=20, col="red") #if collinearity is less than 20, it is generally okay to estimate those parameters


#######################ESTIMATE DATA UNCERTAINTY######################

head(data)
par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(data$NEE~data$time, ylab = "NEE (g C m-2 day-1)", xlab = "Time (days)", col="olivedrab3", pch=16)
plot(data$GPP~data$time, ylab = "GPP (g C m-2 day-1)", xlab = "Time (days)", col="forestgreen", pch=16)
plot(data$Re~data$time, ylab = "Re (g C m-2 day-1)", xlab = "Time (days)", col="red", pch=16)

par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(data$NDVI~data$time, ylab = "NDVI", xlab = "Time (days)", col="lightgreen", pch=16)
plot(data$LAI~data$NDVI, ylab = "LAI", xlab = "NDVI", pch=16)
legend("topleft", legend = "LAI = 0.0471 x exp(8.0783 x NDVI)", bty= "n" )
plot(data$LAI~data$time, ylab = "LAI", xlab = "Time (days)", col="orange", pch=16)


#######################PARAMETER ESTIMATION MCMC###########################

data2 = read.csv("PlantAndSoilData_Assim.csv") #load plant and soil data
head(data) #view other data that is already loaded
head(data2) #view plant and soil data

#Get data ready - need to figure out how to assimilate both types of data at once

#flux data
data.compare1 = data[,c(1,12:15)] #pull out columns that you need
data.compare1 = data.compare1[complete.cases(data.compare1),] #remove rows with NAs
sigma.obs1 = ?????? #observation erros for each data type - data frame with 288 rows and 4 columns corresponding to each data type
#FOR sigma.obs1: columns need to be in SAME ORDER as data.compare1
head(data.compare1)
head(digma.obs1)

#plant/soil data
data.compare2 = data2[,1:8] #pull out columns that you need
sigma.obs2 = data2[,c(1,9:15)] #observation erros for each data type 
#FOR sigma.obs2: columns need to be in SAME ORDER as data.compare2
head(data.compare2) #make sure in same order
head(sigma.obs2)

#other necessary knowns
n.param = 20 #number of parameters, excluding "k" and including initial values of states
M = 100 #number of iterations
D = 11 #number of data types being assimilated (4xflux, 7xplant/soil)

#storage matrices
J = rep(1, M) #storage vector for likelihood ratio
param.est = data.frame(matrix(1, M, n.param)) #storage for parameter estimate iterations; 
colnames(param.est) = c(names(params), names(state))
#change values to the starting values
param.est[,1] = 0.0007 #LitterRate
param.est[,2] = 0.00019 #DecompRate
param.est[,3] = 0.8 #retrans
param.est[,4] = 0.000001 #RespRateSOM
param.est[,5] = 0.00055 #RespRateL
param.est[,6] = 0.08 #kplant
param.est[,7] = 0.1 #Available_N
param.est[,8] = 200 #Biomass_C
param.est[,9] = 3.75 #Biomass_N
param.est[,10] = 200 #Litter_C
param.est[,11] = 3 #Litter_N
param.est[,12] = 2000 #SOM_C
param.est[,13] = 57 #SOM_N

head(param.est) #check to make sure this is correct


#start MCMC
reject = 0 #set reject counter to 0

for (i in 2:M) { #for each iteration
  
  #draw a parameter set from prior distributions
  param.est[i,1] = runif(1, 0.00007, 0.007)
  #param.est[i,2] = runif(1, 0.000019, 0.0019)
  #param.est[i,3] = runif(1, 0.1, 0.99)
  #param.est[i,4] = runif(1, 0.0000001, 0.00001)
  #param.est[i,5] = runif(1, 0.000055, 0.0055)
  #param.est[i,6] = runif(1, 0.008, 0.8)
  #param.est[i,7] = runif(1, 0.01, 10)
  #param.est[i,8] = runif(1, 200,800)
  #param.est[i,9] = runif(1, 1.0, 15)
  #param.est[i,10] = runif(1, 20, 200)
  #param.est[i,11] = runif(1, 0.1, 2)
  #param.est[i,12] = runif(1, 5000, 8000)
  #param.est[i,13] = runif(1, 100, 400)

    
  #run model and calculate error function 
  parms = (param.est[i,1:6]) #parameters for model run
  names(parms) = names(params) #fix names
  state1 = param.est(i,7:13) #initial states for model run
  names(state1) = names(state) #change names
  out = data.frame(solvemodel(parms, state1)) #run model
  #pull out predicted values to compare to data; only include time points where data is available and columns that match data.compare
  out.compare1 = out[match(data.compare1$time, out$time),c(1,10,11,12,9)] 
  out.compare2 = out[match(data.compare2$time, out$time),1:8] 

  j=rep(1, length=D) #to store error calculations for this iteration
  for (d in 1:D) { #for each data type
    if (d < 5) { #for the flux data (data types 1, 2, 3, or 4)
    j[d] = ((data.compare1[,d+1] - out.compare1[,d+1])/sigma.obs1[d+1])^2 #calculate uncertainty weighted error term
    }
    
    if (d > 4) { #for the plant/soil data (data types 5, 6, 7, 8, 9, 10, 11)
      j[d] = ((data.compare2[,d+1] - out.compare2[,d+1])/sigma.obs2[d+1])^2 #calculate uncertainty weighted error term
    }
          
    } #end of data type loop
  
  J[i] = prod(j) #calculate product of all j's - product of error across all data types
    
  ratio = J[i]/J[i-1] #calculate likelihood ratio = J(proposed)/J(current)
  
  u = runif(1,0,1) #draw a random number between 0 and 1
  
  if(ratio < u) {
    reject = reject + 1 #reject and add to rejection counter
    param.est[i,] = param.est[i-1,] #set current values to previous parameter set
    
  }
  
}




