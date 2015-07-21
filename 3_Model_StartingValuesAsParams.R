require(deSolve)
require(FME)

params <- c(kplant = 2,
            LitterRate = 0.0008,
            retrans = 0.85,  
            RespRate = 0.9, 
            UptakeRate = 0.01,
            propN_fol = 0.3,
            propN_roots = 0.5,
            q10 = 2,
            Biomass_C = 650, 
            Biomass_N = 12, 
            SOM_C = 19000, 
            SOM_N = 800,
            Available_N = 0.1
            )

#state <- c(Biomass_C = 400, 
#           Biomass_N = 4.5, 
#           SOM_C = 1600, 
#           SOM_N = 35,
#           Available_N = 0.1)

time = seq(1, 1826, 1)


####################MODEL#################################

solvemodel <- function(params, times) {
  
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
      k=0.63
      Pmax =1.18
      E0 = 0.03
      cue = 0.5
      Ndep_rate = 0.0004 #calculated from LTER data
      
      
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
            
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI * scal ) ) ) * 12 
      Uptake =  UptakeRate * (Biomass_C*propN_roots) * ( Available_N / ( kplant + Available_N ) ) * scal
      Ra =  ( 1 - cue ) * GPP
      Re = RespRate * (q10 ^ ( ( Temp - 10 )/ 10 ) )
      Rh = Re - Ra
      #Ntrans = netNrate * ( q10 ^ ( (Temp-10) / 50 ) )
      if(PAR==0){
        Ntrans = 3.3E-9 * SOM_C * 2.5
      } else {
        Ntrans = 4E-8 * SOM_C * 2.5
      }
            
      N_dep = Ndep_rate
      Litterfall_N  =  LitterRate * Biomass_N * ( 1 - retrans )
      Litterfall_C =  LitterRate * Biomass_C
      
      
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
  
  
  return(ode(y=params[9:13],times=time,func=model,parms = params[1:8], method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params)) #creates table of model output
