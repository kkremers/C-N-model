require(deSolve)
require(FME)

params <- c(kplant = 0.2, #0.07-0.34
            LitterRate = 0.0007, #0.0001-0.0024
            UptakeRate = 0.008, #0.002-0.012
            propN_fol = 0.1, #0.1-0.9
            propN_roots = 0.015, #0.002-0.015
            netNrate = 0.015, #0.001-0.04
            Biomass_C = 684.5, 
            Biomass_N = 12.9, 
            SOM_C = 19358.7, 
            SOM_N = 854.1,
            Available_N = 1.6)


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
      Nfix_rate=0.0015 #calculated from Alaska's changing arctic pg 106      
      R0=0.06 #0.07
      beta=0.06 #0.07
      Rx=0.02 #0.01
      Pmax = 1.2 #1.16 
      E0 = 0.03 #0.02
      q10=1.9 #1.92
      k = 0.81
      
      
      #calculate propN_fol
      propN_fol.T = propN_fol + (0.0078*Temp_avg)
      
      #FLUXES
      TFN=propN_fol.T*Biomass_N 
      
      LAI = ((TFN-0.31)/1.29) * scalseason #Williams and Rastetter 1999
      
      NDVI=0
      if(LAI>0 & !is.na(LAI)){
        NDVI = (log(LAI/0.0026)/8.0783)
      }        
      
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI) ) ) * 12
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) )
      Ra =  12*((R0*LAI)*exp(beta*Temp))
      Rh = 12*((Rx)*exp(beta*Temp))
      Re = Ra+Rh
      Ntrans = netNrate * ( q10 ^ ( (Temp-10) / 10 ) )
      N_dep = Ndep_rate
      N_fix=Nfix_rate*scaltemp
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
      dAvailable_N = Ntrans - Uptake + N_dep + N_fix
      
      
      #what to output
      
      list(c(dBiomass_C, 
             dBiomass_N, 
             dSOM_C, 
             dSOM_N,
             dAvailable_N), 
           c(NEE=NEE, GPP=GPP, Re=Re, LAI=LAI, NDVI=NDVI, Ra=Ra, Rh=Rh, Uptake = Uptake, 
             Ntrans=Ntrans, N_fix=N_fix, Litterfall_C=Litterfall_C, Litterfall_N=Litterfall_N, 
             DOY=DOY, TFN=TFN, Temp=Temp, scaltemp = scaltemp, scalseason = scalseason, year=Year, propN_fol = propN_fol.T, NDVI_MODIS=NDVI_MODIS))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=params[7:11],times=time,func=model,parms = params[1:6], method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out= data.frame(solvemodel(params)) #creates table of model output
