require(deSolve)
require(FME)


params <- c(kplant = 0.16, #0.07-0.34
            LitterRate = 0.0008, #0.0001-0.0024
            UptakeRate = 0.005, #0.002-0.012
            propN_fol = 0.1, #0.01-0.23
            propN_roots = 0.01, #0.01-0.022
            netNrate = 0.007, #0.003-0.04
            q10=1.29, #1-3
            Biomass_C = 500, 
            Biomass_N = 10, 
            SOM_C = 16000, 
            SOM_N = 800,
            Available_N = 1)

####################MODEL#################################

solvemodel <- function(params, times) {
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      Temp_avg = TempAvg.d1(t)
      PAR=PAR.d1(t)
      DOY = DOY.d1(t)
      scaltemp=scaltemp.d1(t)
      scalseason=scalseason.d1(t)
      Year = Year.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      Ndep_rate = 0.00007 #calculated from Alaska's changing arctic pg 106
      Nout_rate = 0.0002 #calculated from Alaska's changing arctic page 106
      Nfix_rate=0.001 #calculated from Alaska's changing arctic pg 106 and scal.temp   
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
      
      GPP = LAI*((Pmax*E0*PAR)/(Pmax+(E0*PAR))) * 12
      Ra = ((R0*LAI)*exp(beta*Temp)) * 12
      Rh = (Rx*exp(beta*Temp)) * 12 
      Re = Ra+Rh
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) )
      Ntrans = netNrate * q10^(Temp/10)
      Litterfall_C =  LitterRate * Biomass_C
      Litterfall_N  =  LitterRate * Biomass_N
      
      #calculated variables to use for model fitting and analysis
      NEE = Re - GPP
      
      NDVI_MODIS = (1.23846*NDVI)-0.14534
      
      #differential equations
      dBiomass_C = GPP  - Ra  - Litterfall_C 
      dBiomass_N = Uptake  - Litterfall_N 
      dSOM_C = Litterfall_C  - Rh
      dSOM_N = Litterfall_N - Ntrans
      dAvailable_N = Ntrans - Uptake
      
      
      #what to output
      
      list(c(dBiomass_C, 
             dBiomass_N, 
             dSOM_C, 
             dSOM_N,
             dAvailable_N), 
           c(NEE=NEE, GPP=GPP, Re=Re, LAI=LAI, NDVI=NDVI, Ra=Ra, Rh=Rh, Uptake = Uptake, 
             Ntrans=Ntrans, Litterfall_C=Litterfall_C, Litterfall_N=Litterfall_N, 
             DOY=DOY, TFN=TFN, Temp=Temp, scaltemp = scaltemp, scalseason = scalseason, year=Year, propN_fol = propN_fol.T, NDVI_MODIS=NDVI_MODIS))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=params[8:12],times=time,func=model,parms = params[1:8], method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out= data.frame(solvemodel(params)) #creates table of model output
