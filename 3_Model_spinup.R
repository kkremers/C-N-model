data = read.csv("InputData_Processed.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data)


#subset for years of interest (2009-2012)
data=data[data$year!=2013,]
head(data)
tail(data)

#plot the data
par(mfrow=c(2,2), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$GDD~data$time, ylab = "Growing Degree Days (GDD) ",  xlab="", col="forestgreen")
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

num.years = 20


#calculate daily average temperature
Temp_avg = tapply(data$Temp_ARF, data$DOY, mean, na.rm=TRUE)
plot(Temp_avg, type="l")
Temp_spin = rep(Temp_avg, num.years)

#calculate daily average PAR
PAR_avg = tapply(data$PAR_vis, data$DOY, mean, na.rm=TRUE)
plot(PAR_avg, type="l")
PAR_spin = rep(PAR_avg, num.years)

#calculate average GDD
GDD_avg = tapply(data$GDD, data$DOY, mean, na.rm=TRUE)
plot(GDD_avg, type="l")
GDD_spin = rep(GDD_avg, num.years)

#calculate GDD slope
GDD.slope = rep(0, length = length(GDD_spin))
for (i in 2: length(GDD_spin)){
  if(Temp_spin[i]>0){
  GDD.slope[i] = Temp_spin[i]}
  if(Temp_spin[i]<=0){
    GDD.slope[i]=0
  }
}
plot(GDD.slope, type="l")

#create DOY vector
DOY_spin=rep(1:366,num.years)

#calculate scalars

scal.temp=NULL
for (i in 1:length(GDD.slope)){
  scal.temp[i] = (GDD.slope[i] - 0)/(max(GDD.slope)-0) #growing degree day scalar
}
plot(scal.temp, type="l")

scal.GDD=NULL
for (i in 1:length(GDD_spin)){
  scal.GDD[i] = (GDD_spin[i] - 0)/(max(GDD_spin)-0) #growing degree day scalar
  if(DOY_spin[i]>250){
    scal.GDD[i]=0
  }
}
plot(scal.GDD, type="l")

#what if we add the GDD scalar and the smoothed temp scalar together?
scal.new = scal.temp+scal.GDD
#rescale to 1
scal.add=NULL
for (i in 1:length(scal.new)){
  scal.add[i] = (scal.new[i] - min(scal.new))/(max(scal.new)-min(scal.new)) #growing degree day scalar
}

par(mfrow=c(3,1), mar=c(4,4,0.5,2))
plot(scal.GDD, type="l")
plot(scal.temp, type="l")
plot(scal.add, type="l")

time=seq(1:length(Temp_spin))

#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=time, y=Temp_spin, method="linear", rule=2)
PAR.d1 <- approxfun(x=time, y=PAR_spin, method="linear", rule=2)
scalGDD.d1 <- approxfun(x=time, y=scal.GDD, method="linear", rule=2)
scaltemp.d1 <- approxfun(x=time, y=scal.temp, method="linear", rule=2)
scaladd.d1 <- approxfun(x=time, y=scal.add, method="linear", rule=2)
DOY.d1 <- approxfun(x=time, y=DOY_spin, method="linear", rule=2)


####################MODEL################################

require(deSolve)
require(FME)

params <- c(kplant = 0.2, #0.07-0.34
            LitterRateC = 0.00035, #0.0001-0.0009
            LitterRateN = 0.001, #0.0001-0.0024 
            RespRate = 0.96, #0.26-0.98
            UptakeRate = 0.012, #0.002-0.012
            propN_fol = 0.1, #0.1-0.9
            propN_roots = 0.01, #0.002-0.015
            q10 = 2, #1.4-3.3
            netNrate = 0.02, #0.001-0.04
            cue = 0.3 #0.25-0.7
)

state <- c(Biomass_C = 684.5, 
           Biomass_N = 12.9, 
           SOM_C = 19358.7, 
           SOM_N = 854.1,
           Available_N = 1.6)


solvemodel_sp <- function(params, state, times) {
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      PAR=PAR.d1(t)
      DOY = DOY.d1(t)
      scal=scaladd.d1(t)
      scalGDD=scalGDD.d1(t)
      scaltemp=scaltemp.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      Ndep_rate = 0.0004 #calculated from LTER data
      k=0.63
      Pmax =1.18
      E0 = 0.03
      temp2_resp = 10
      temp2_netn = 10
      
      #FLUXES
      TFN=propN_fol*Biomass_N
      
      LAI = ((TFN-0.31)/1.29) * scaltemp #Williams and Rastetter 1999
      if(PAR==0 | LAI<=0){
        LAI=0
      }
      
      NDVI=0
      if(LAI>0){
        NDVI = log((LAI)/0.0026)/8.0783
      }
      
      if(NDVI==-Inf){
        NDVI=0
      }
      
      
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI ) ) ) * 12 
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) ) * scaltemp
      Ra =  ( 1 - cue ) * GPP
      Re = RespRate * (q10 ^ ( ( Temp - temp2_resp)/ 10 ) )
      Rh = Re - Ra
      Ntrans = netNrate * ( q10 ^ ( (Temp-temp2_netn) / 10 ) )
      N_dep = Ndep_rate
      Litterfall_N  =  LitterRateN * Biomass_N
      Litterfall_C =  LitterRateC * Biomass_C
      
      
      #calculated variables to use for model fitting and analysis
      NEE = Re - GPP
      
      #differential equations
      dBiomass_C = GPP  - Ra  - Litterfall_C 
      dBiomass_N = Uptake  - Litterfall_N 
      dSOM_C = Litterfall_C  - Rh
      dSOM_N = Litterfall_N - Ntrans
      dAvailable_N = Ntrans - Uptake + N_dep
      
      
      #what to output
      
      list(c(dBiomass_C, 
             dBiomass_N, 
             dSOM_C, 
             dSOM_N,
             dAvailable_N), 
           c(NEE=NEE, GPP=GPP, Re=Re, LAI=LAI, NDVI=NDVI, Ra=Ra, Rh=Rh, Uptake = Uptake, 
             Ntrans=Ntrans, Litterfall_C=Litterfall_C, Litterfall_N=Litterfall_N, 
             scalGDD=scalGDD, scaltemp=scaltemp, DOY=DOY))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel_sp(params, state)) #creates table of model output
