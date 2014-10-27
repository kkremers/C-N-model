####Load packages
require(deSolve)
require(FME)



####################################

#using real data

#first upload and viewdata 

data = read.csv("OriginalInputData.csv") #choose file from directory
names(data) = c("time", "date", "year", "DOY", "Albedo", "Temp_T", "PAR_T", "Temp_ARF", "PAR_ARF", "EVI", "NDVI", "LAI", "NEE", "Re", "GPP")
head(data) #view table

#plot data
par(mfrow=c(2,1)) 
plot(data$Temp_T~data$time, type="l", ylab = "Daily Max Temp (C)") 
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16)
plot(data$PAR_T~data$time, type="l", ylab = "Daily Max PAR (umol m-2 s-1)")
points(data$PAR_ARF~data$time, col="blue", pch=16)
#ARF data is missing winter measurements - want to fill this in

#want to plot to determine relationship, and then fill in missing data in ARF data
#start with temperature
linmod.t = lm(data$Temp_ARF~data$Temp_T + 0) #linear model, set intercept = 0
summary(linmod.t) #summary
slope.t = summary(linmod.t)$coefficients[1,1] #slope value

par(mfrow=c(1,1))
plot(data$Temp_ARF~data$Temp_T, ylab = "ARF", xlab="Toolik")
abline(linmod.t) #linear regression line

#do the same thing for PAR data
linmod.p = lm(data$PAR_ARF~data$PAR_T + 0)
summary(linmod.p)
slope.p = summary(linmod.p)$coefficients[1,1]

par(mfrow=c(1,1))
plot(data$PAR_ARF~data$PAR_T, ylab = "ARF", xlab="Toolik")
abline(linmod.p)

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
plot(data$Temp_T~data$time, type="l", ylab = "Daily Max Temp (C)")
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16)
plot(data$PAR_T~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)")
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

data = data.frame(data[,1:9], PAR_vis = PAR_vis, data[,10:15])
head(data)


#check output
par(mfrow=c(2,1))
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", main="All PAR")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", main="Plant Available PAR")


write.csv(data, "FluxData.csv") #added the updated data to the working directory so that it is easy to access - won't have to do any of the above steps again
##################################################

############re-loading the data



############re-loading the data

#set working directory to C-N-model
data = read.csv("FluxData.csv")
head(data)

#plot the data
par(mfrow=c(2,2), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$Albedo~data$time, type="l", ylab = "Albedo", ylim=c(0,1.2), xlab="")
abline(h=0.2, col="red", lty=2)
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")



#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_vis, method="linear", rule=2)
NDVI.d1 <- approxfun(x=data$time, y=data$NDVI, method="linear", rule=2)


######################Parameters##########################
params <- c(LitterRate = 0.0014,
            DecompRateC = 0.00008, 
            DecompRateN = 0.00017,
            retrans = 0.9,  
            RespRateSOM = 1E-6, 
            RespRateL = 2.5E-3,
            FCM = 130,
            Uptake5 = 0.003,
            UptakeRate = 0.01,
            kplant = 0.11,
            q10 = 2,
            E0 = 0.03, 
            k = 0.5, 
            Pmax = 1.5)


####################MODEL#################################
time = seq(1, 1461, 1)

solvemodel <- function(params, times=time) {
  
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      PAR=PAR.d1(t)
      NDVI=NDVI.d1(t)
      
      NDVI.max = 0.694613   #max(data$NDVI, na.rm=TRUE)
      NDVI.min = 0 #min(data$NDVI, na.rm=TRUE)
      
      #FLUXES
      s.ndvi = (NDVI - NDVI.min)/(NDVI.max-NDVI.min)
      LAI = ((Biomass_C*0.4)/FCM)*s.ndvi
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI ) ) ) * 12 
      Uptake =  UptakeRate * LAI * ( Available_N / ( kplant + Available_N ) )
      cue = (Uptake - 0)/(Uptake5 - 0)*0.5
      Immobilization =   0.0013 * (q10 ^ (Temp / 70))
      Rh1 =  RespRateL * Litter_C * ( q10 ^ ( Temp / 10 ) )
      N_deposition =  0.0017
      Rh2 =  RespRateSOM * SOM_C * ( q10 ^ ( Temp / 10 ) )
      Decomp_C =  DecompRateC * Litter_C * ( q10 ^ ( Temp / 10 ) )
      Decomp_N =  DecompRateN* Litter_N * ( q10 ^ ( Temp / 10 ) )
      Litterfall_N =  LitterRate * Biomass_N * ( 1 - retrans )
      Ra =  ( 1 - cue ) * GPP
      Litterfall_C =  LitterRate * Biomass_C
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
             Uptake = Uptake, s.ndvi=s.ndvi, Immobilization=Immobilization,
             Mineralization = Mineralization))
      
    })  #end of with(as.list(...
  } #end of model
  
  #STATE VARIABLES
state = c(Available_N = 0.1, 
          Biomass_C = 200, 
          Biomass_N = 3.5, 
          Litter_C = 110, 
          Litter_N = 3, 
          SOM_C = 2000, 
          SOM_N = 57)

  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params)) #creates table of model output
#

##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(3,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$NDVI~data$time, type="l", ylab = "NDVI", col="springgreen3", xlab = "")
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

par(mfrow=c(3,1), mar=c(4,5,2,2))
plot(out$s.ndvi~data$NDVI, xlab = "NDVI", ylab = "Scalar (s.ndvi)", cex.lab = 1.5)
plot(out$LAI~data$NDVI, xlab = "NDVI", ylab = "LAI",cex.lab = 1.5)
plot(out$LAI~out$Biomass_C, xlab = "Biomass C", ylab = "LAI", cex.lab = 1.5)

head(out)



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

s.global <- sensRange(func=solvemodel, parms=params, dist ="unif", 
                      sensvar = sensvars, parRange=parRanges, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "day", ylab = "g/m2", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=params, senspar = names(params), 
                   sensvar = sensvars, varscale = 1)

head(s.local)
s.local.summ = summary(s.local, var=T)
s.loc.summ.ordered = s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] 
write.csv(s.loc.summ.ordered, "c:/Users/Rocha Lab/Desktop/Kelsey/LocalSensitivityAnalysis.csv")
param.cor = data.frame(cor(s.local[,c(-1,-2)]))#table of parameter correlations
write.csv(param.cor, "c:/Users/Rocha Lab/Desktop/Kelsey/ParamCorr.csv")
windows()
plot(param.cor)

plot(s.local)


#######################ESTIMATE DATA UNCERTAINTY######################

head(data)

#######################PARAMETER ESTIMATION###########################

#need to write MCMC



