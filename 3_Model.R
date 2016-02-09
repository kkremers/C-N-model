require(deSolve)
require(FME)


params <- c(kplant = 0.1, #0.07-0.34
            LitterRate = 0.0005, #0.0001-0.0024
            UptakeRate = 0.002, #0.0020-0.004
            propN_fol = 0.05, #0-0.8
            propN_roots = 0.01, #0.002-0.03
            netNrate = 0.008, #0.001-0.04
            cue=0.7, #0.4-0.8
            BiomassCN = 48) #28-62

state  <- c(Biomass_C = 400, 
            Biomass_N = 7.5, 
            SOM_C = 9000, 
            SOM_N = 257,
            Available_N = 1)


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
      scaltemp=scaltemp.d1(t)
      scalseason=scalseason.d1(t)
      Year = Year.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      Ndep_rate = 0.00007 #calculated from Alaska's changing arctic pg 106
      Nfix_rate=0.0015 #calculated from Alaska's changing arctic pg 106      
      R0=0.07
      beta=0.07
      Rx=0.01
      Pmax = 1.16 
      E0 = 0.02
      q10=1.92 
      
      #calculate propN_fol
      propN_fol.T = propN_fol + (0.0078*Temp_avg)
      
      #FLUXES
      TFN=propN_fol.T*Biomass_N 
      
      LAI = ((TFN-0.31)/1.29) * scalseason #Williams and Rastetter 1999
      
      NDVI=0
      if(LAI>0){
        NDVI = (log(LAI/0.0026)/8.0783)
      }      
      
      GPP = LAI * ( ( Pmax * E0 * PAR ) / ( Pmax + E0 * PAR ) ) * 12
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) )
      Ra =  ( 1 - cue ) * GPP
      Re = ((R0*LAI)+Rx)*exp(beta*Temp)*12  
      Rh = Re - Ra
      Ntrans = netNrate * ( q10 ^ ( (Temp-10) / 10 ) )
      N_dep = Ndep_rate
      N_fix=Nfix_rate*scaltemp
      Litterfall_C =  LitterRate * Biomass_C
      Litterfall_N  =  Litterfall_C * (1/BiomassCN)
      
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
  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out= data.frame(solvemodel(params, state)) #creates table of model output
