require(deSolve)
require(FME)

#best parameter set and starting values from model optimization on June 11, 2016
params <- c(kplant = 0.104, #0.07-0.34
            LitterRate = 0.001599, #0.0001-0.0024
            UptakeRate = 0.0103, #0.002-0.012
            propN_fol = 0.129, #0.01-0.23
            propN_roots = 0.0209, #0.01-0.022
            netNrate = 1.053E-5, #6.25E-8 to 1.875E-5
            q10 = 2.8417) #1 - 3

state  <- c(Biomass_C = 196.61, 
            Biomass_N = 5.387, 
            SOM_C = 20501.8, 
            SOM_N = 808.17,
            Available_N = 0.0024)

####################MODEL#################################

solvemodel <- function(params, state, times) {
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      Temp_avg = TempAvg.d1(t)
      PAR=PAR.d1(t)
      DOY = DOY.d1(t)
      scalseason= scalseason.d1(t)
      Year = Year.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      Nin_rate = 0.00034 #calculated from Alaska's changing arctic pg 106
      Nout_rate = 0.0002 #calculated from Alaska's changing arctic page 106
      R0= 0.07 #0.045-0.105
      beta= 0.07 #0.05-0.09
      Rx=0.01 #0.001-0.04
      Pmax = 1.16 #1.08-1.24
      E0 = 0.02 #0.015-0.035
      
      #calculate propN_fol
      propN_fol.T = propN_fol + (0.0078*Temp_avg)
      
      #FLUXES
      TFN=propN_fol.T*Biomass_N 
      
      LAI = ((TFN-0.31)/1.29) * scalseason #Williams and Rastetter 1999
      
      NDVI=0
      if(LAI>0 & !is.na(LAI)){
        NDVI = (log(LAI/0.008)/8.0783)
      }   
      if(NDVI<0){
        NDVI = 0
      }
      
      GPP = LAI*((Pmax*E0*PAR)/(Pmax+(E0*PAR))) * 12
      Ra = ((R0*LAI)*exp(beta*Temp)) * 12
      Rh = (Rx*exp(beta*Temp)) * 12 
      Re = Ra+Rh
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) )
      Ntrans = netNrate * SOM_N * q10^(Temp/10)
      Litterfall_C =  LitterRate * Biomass_C
      Litterfall_N  =  LitterRate * Biomass_N
      N.out = Nout_rate
      N.in =  Nin_rate
      
      #calculated variables to use for model fitting and analysis
      NEE = Re - GPP
      
      
      #differential equations
      dBiomass_C = GPP  - Ra  - Litterfall_C 
      dBiomass_N = Uptake  - Litterfall_N 
      dSOM_C = Litterfall_C  - Rh
      dSOM_N = Litterfall_N - Ntrans
      dAvailable_N = Ntrans + N.in + - Uptake - N.out
      
      
      #what to output
      
      list(c(dBiomass_C, 
             dBiomass_N, 
             dSOM_C, 
             dSOM_N,
             dAvailable_N), 
           c(NEE=NEE, GPP=GPP, Re=Re, LAI=LAI, NDVI=NDVI, Ra=Ra, Rh=Rh, Uptake = Uptake, 
             Ntrans=Ntrans, Litterfall_C=Litterfall_C, Litterfall_N=Litterfall_N, Tavg=Temp_avg,
             DOY=DOY, TFN=TFN, Temp=Temp, PAR=PAR, scalseason = scalseason, year=Year, propN_fol = propN_fol.T))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out= data.frame(solvemodel(params, state)) #creates table of model output

