require(deSolve)
require(FME)

params <- c(kplant = 0.11,
            LitterRate = 0.0025,
            DecompRateC = 0.005,
            DecompRateN = 0.0007,
            retrans = 0.8,  
            RespRate = 1, 
            UptakeRate = 0.0001,
            netNrate = 0.0008,
            q10 = 2
            )

state <- c(Biomass_C = 400, 
           Biomass_N = 4.5, 
           Litter_C = 160, 
           Litter_N = 1.6, 
           SOM_C = 2000, 
           SOM_N = 56,
           Available_N = 0.1)

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
      DOY.sen = DOYsen.d1(t)
      #scalTEMP=scaltemp.d1(t) 
      scal=scaladd.d1(t)
      scalGDD=scalGDD.d1(t)
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      k=0.63
      Pmax =1.18
      E0 = 0.03
      cue = 0.5
      propN_fol = 0.3
      propN_roots = 0.5
      
      
      #FLUXES
      TFN=propN_fol*Biomass_N
      
      LAI = ((TFN-0.31)/1.29) #Williams and Rastetter 1999
      if(PAR==0 | LAI<=0){
        LAI=0
      }
      
      NDVI=0
      if(LAI>0){
      NDVI = log((LAI*scalGDD)/0.003)/7.845}
            
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI *scal ) ) ) * 12 
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) ) * scal
      Ra =  ( 1 - cue ) * GPP
      Re = RespRate * (q10 ^ ( ( Temp - 10 )/ 10 ) )
      Rh = Re - Ra
      Decomp_C = DecompRateC * Litter_C * ( q10 ^ ( (Temp-10) / 10 ) )
      Decomp_N = DecompRateN * Litter_N * ( q10 ^ ( (Temp-10) / 10 ) )
      Ntrans = netNrate * ( q10 ^ ( (Temp-10) / 50 ) )
            
      N_dep = 0.00008
      Litterfall_N  =  LitterRate * Biomass_N * ( 1 - retrans )
      Litterfall_C =  LitterRate * Biomass_C
      
      if(DOY < DOY.sen){
        Litterfall_N = 0
        Litterfall_C = 0
      }
      
      
      #calculated variables to use for model fitting and analysis
      NEE = Re - GPP
      
      #differential equations
      dBiomass_C = GPP  - Ra  - Litterfall_C 
      dBiomass_N = Uptake  - Litterfall_N 
      dLitter_C = Litterfall_C  - Decomp_C 
      dLitter_N = Litterfall_N  - Decomp_N 
      dSOM_C = Decomp_C  - Rh
      dSOM_N = Decomp_N  + N_dep - Ntrans
      dAvailable_N = Ntrans - Uptake
      
      
      #what to output
      
      list(c(dBiomass_C, 
             dBiomass_N, 
             dLitter_C, 
             dLitter_N, 
             dSOM_C, 
             dSOM_N,
             dAvailable_N), 
             c(GPP=GPP, LAI=LAI, NDVI=NDVI, NEE=NEE, Re=Re, Ra=Ra, Rh=Rh, Uptake = Uptake, 
             Ntrans=Ntrans, Litterfall_C=Litterfall_C, Litterfall_N=Litterfall_N, 
             Decomp_C = Decomp_C, Decomp_N = Decomp_N, scalGDD=scalGDD, scal=scal, DOY=DOY))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=state,times=time,func=model,parms = params, method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params, state)) #creates table of model output
