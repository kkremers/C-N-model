require(deSolve)
require(FME)

params <- c(kplant = 0.2, #0.07-0.34
            LitterRate = 0.0007, #0.0001-0.0024
            UptakeRate = 0.008, #0.002-0.012
            propN_fol = 0.1, #0.1-0.9
            propN_roots = 0.015, #0.002-0.015
            netNrate = 0.015, #0.001-0.04
            cue=0.7, #0.4-0.8
            beta=0.05,
            Biomass_C = 684.5, 
            Biomass_N = 12.9, 
            SOM_C = 19358.7, 
            SOM_N = 854.1,
            Available_N = 1.6)

time = seq(1, 1461, 1)

####################MODEL#################################

solvemodel <- function(params, times) {
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      PAR=PAR.d1(t)
      albedo = albedo.d1(t)
      DOY = DOY.d1(t)
      DOYpeak = DOYpeak.d1(t)
      scaltemp=scaltemp.d1(t)
      scalseason=scalseason.d1(t)
      year = Year.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      Ndep_rate = 0.00007 #calculated from Alaska's changing arctic pg 106
      Nfix_rate=0.0015 #calculated from Alaska's changing arctic pg 106
      k=0.5
      Pmax = 1.16 
      E0 = 0.03
      q10=1.9
      R0 = 0.06 
      Rx = 0.02 
      SOM_CN = 36
      
      
      #FLUXES
      TFN=propN_fol*Biomass_N
      
      LAI = ((TFN-0.31)/1.29) * scalseason #Williams and Rastetter 1999
      
      
      NDVI=0
      if(LAI>0){
        NDVI = (log(LAI/0.0026)/8.0783)
      }      
      
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI) ) ) * 12
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) )
      Ra =  ( 1 - cue ) * GPP
      Re = ((R0*LAI)+Rx)*exp(beta*Temp)*12  
      Rh = Re - Ra
      Ntrans = netNrate * ( q10 ^ ( (Temp-10) / 10 ) )
      N_dep = Ndep_rate
      N_fix=Nfix_rate*scaltemp
      Litterfall_C =  LitterRate * Biomass_C
      Litterfall_N  =  LitterRate * Biomass_N
      
      #calculated variables to use for model fitting and analysis
      NEE = Re - GPP
      
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
             DOY=DOY, year=year, TFN=TFN, Temp=Temp, scaltemp = scaltemp, scalseason = scalseason))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=params[9:13],times=time,func=model,parms = params[1:8], method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out= data.frame(solvemodel(params)) #creates table of model output

