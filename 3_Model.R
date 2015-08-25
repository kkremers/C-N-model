require(deSolve)
require(FME)

params <- c(kplant = 0.2, #0.07-0.34
            LitterRateC = 0.00035, #0.0001-0.0024
            LitterRateN = 0.0015, #0.0001-0.0024 
            RespRate = 0.96, #0.26-0.98
            UptakeRate = 0.002, #0.002-0.012
            propN_fol = 0.1, #0.1-0.9
            propN_roots = 0.5, #0.1-0.9
            q10 = 2, #1.4-3.3
            netNrate = 0.03, #0.001-0.1
            cue = 0.3 #0.25-0.7
            )

state <- c(Biomass_C = 684.5, 
           Biomass_N = 12.9, 
           SOM_C = 19358.7, 
           SOM_N = 854.1,
           Available_N = 1.6)

time = seq(1, 1826, 1)


####################MODEL#################################

solvemodel <- function(params, state, times) {
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      PAR=PAR.d1(t)
      DOY = DOY.d1(t)
      scal=scaladd.d1(t)
      scalGDD=scalGDD.d1(t)
      year = Year.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      Ndep_rate = 0.0004 #calculated from LTER data
      k=0.63
      Pmax =1.18
      E0 = 0.03
      temp2_resp = 10
      temp2_netn = 10
      
      #FLUXES
      TFN=propN_fol*Biomass_N
      
      LAI = ((TFN-0.31)/1.29) #Williams and Rastetter 1999
      if(PAR==0 | LAI<=0){
        LAI=0
      }
      
      NDVI=0
      if(LAI>0){
      NDVI = log((LAI*scalGDD)/0.003)/7.845}
      
      if(NDVI==-Inf){
        NDVI=0
      }
            
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI * scal) ) ) * 12 
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) ) * scal
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
             scalGDD=scalGDD, scal=scal, DOY=DOY, year=year))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params, state)) #creates table of model output
