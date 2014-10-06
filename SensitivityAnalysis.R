
install.packages("FME") #package for sensitivity analysis
library(FME) 
library(deSolve) #package for ODE

numyears = 2
TIME=365*numyears
time=seq(1,TIME,1)


#using simulated data
gaussian <- function(x, a, b, c) {  #x is data, a is height, b is peak position (mean), c is width (standard deviation)
  a * exp(-((x-b)^2)/(2*(c^2))) }

Temp=gaussian(1:365, 40, 195, 70)-20
Temp=rep.int(Temp, numyears)

PAR=gaussian(1:365, 600, 170, 60)
PAR=rep.int(PAR, numyears)

LAI=gaussian(1:365, 1.2, 175, 35)
LAI=rep.int(LAI, numyears)

#make noisy
for (i in 1:TIME){
  Temp[i]=Temp[i]+rnorm(1,0,2)}


for (i in 1:TIME){
  PAR[i]=PAR[i]+rnorm(1,0,15)
  if(PAR[i]<0){PAR[i]=0}}

for (i in 1:TIME){
  LAI[i]=LAI[i]+rnorm(1,0,0.05)
  if(LAI[i]<0){LAI[i]=0}}

set.seed(1)

pmax = 14.747 #umolCO2/gN*S; maximum photosynthetic rate
E0 = 0.041 #umolCO2/umolPAR;light use efficiency
k = 0.5#m2/m2; Beer's law extinction coefficient
GPP = (pmax/k)*log((pmax+E0*PAR)/(pmax+E0*PAR*exp(-k*LAI)))*(10^-6)*12*86400
for(i in 1:TIME){
  if(GPP[i]<0){GPP[i]=0}}

par(mfrow=c(2,2))
plot(GPP, type="l")
plot(PAR, type="l")
plot(Temp, type="l")
plot(LAI, type="l")


#parameters
q10 = 2 #q10
t1 = 0.05 #Rh constant
t2 = 0.007 #Litter rate constant; Hobbie 2002
t3 = 0.03 #Decomp rate constant
t4 = 0.8 #constant representing plant and microbial competition for N
t5 = 0.2 #microbial carbon uptake constant
t6 = 0.05 #microbial nitrogen uptake constant
t7 = 5 #plant nitrogen uptake constant 
t8 = 0.003 #microbial death rate
t9 = 0.05 #plant half saturation constant for N uptake
t10 = 0.1 #microbial half saturation constant for N and C uptake

params <- list(q10=q10, t1=t1, t2=t2, t3=t3, t4=t4, t5=t5, t6=t6, t7=t7, t8=t8, t9=t9, t10=t10)


Temp.d1 <- approxfun(x=time, y=Temp, method="linear", rule=2)
GPP.d1 <- approxfun(x=time, y=GPP, method="linear", rule=2)


solvemodel <- function(params, times=time) {
model<-function(t,state,params)
{ 
  with(as.list(c(state, params)),{ #start of with(as.list(...
    
    Temp=Temp.d1(t)
    GPP=GPP.d1(t)
    
    
    #FLUXES
    s=1/(1+exp(-Temp))
    U = (t7*(PLANT.C)*(NITROGEN/(t9+NITROGEN)))*s
    k=0.01*exp(-0.01*U)
    cue=(U/(k+U))/1.5
    NPP = cue*GPP #cue*GPP
    Ra = (1-cue)*GPP
    
    
    L.C = t2*PLANT.C
    L.N = t2*PLANT.N
    
    D.C = t3*LITTER.C*q10^(Temp/10)
    D.N = t3*LITTER.N*q10^(Temp/10)
    
    Mic.C = t5*NFIXMIC.C*(SOM.C/(t10+SOM.C))*s
    Mic.N = t6*NFIXMIC.N*(SOM.N/(t10+SOM.N))*s
    A = t4*Mic.N
    
    Min = 0.0018*(q10^(Temp/10)) #gN/m2*day mineralization rate per day; Hobbie 2002
    Nfix.ml = 0.004*(q10^(Temp/10)) #gN/m2*day #rate of N fixed by mosses and lichens; Porada 2013; could be conservative estimate
    Nfix.f = 0.0025*(q10^(Temp/10)) #gN/m2*day rate of N fixed by fungi/mycorrhizae; Hobbie 2006
    death.C=t8*NFIXMIC.C
    death.N=t8*NFIXMIC.N    
    Ndep = 0.00015 #gN/m2*day deposition rate
    
    Rh = t1*NFIXMIC.C*q10^(Temp/10)
    Re= Ra+Rh
    
    
    #differential equations
    dPLANT.C = NPP-L.C
    dPLANT.N = U-L.N
    dLITTER.C = L.C-D.C
    dLITTER.N = L.N-D.N
    dSOM.C = D.C+death.C-Mic.C
    dSOM.N = D.N+death.N+Ndep-Mic.N-A
    dNFIXMIC.C=Mic.C-death.C-Rh
    dNFIXMIC.N=Mic.N-death.N-Nfix.ml-Nfix.f-Min
    dNITROGEN = A+Nfix.ml+Nfix.f+Min-U
    
    #what to output
    
    list(c(dPLANT.C,
           dPLANT.N,
           dLITTER.C,
           dLITTER.N,
           dSOM.C,
           dSOM.N,
           dNFIXMIC.C,
           dNFIXMIC.N,
           dNITROGEN
           
    ))
      
  })  #end of with(as.list(...
} #end of model


#STATE VARIABLES
PLANT.C = 200
PLANT.N = 5
LITTER.C = 30
LITTER.N = 1.5
SOM.C = 100
SOM.N = 4
NFIXMIC.C=20
NFIXMIC.N=3
NITROGEN = 0.04

state = c(PLANT.C = PLANT.C,
          PLANT.N = PLANT.N,
          LITTER.C = LITTER.C,
          LITTER.N = LITTER.N,
          SOM.C = SOM.C,
          SOM.N = SOM.N,
          NFIXMIC.C = NFIXMIC.C,
          NFIXMIC.N = NFIXMIC.N,
          NITROGEN = NITROGEN)


return(ode(y=state,times=time,func=model,parms = params)) #integrate

} #end of solve model

out = solvemodel(params) #creates table of model output
head(out)

parRanges = data.frame(
            min = c(1.5, 0.1, 0.001, 0.01, 0.1, 0.05, 0.01, 1, 0.0005, 0.01, 0.05),
            max = c(3, 1, 0.01, 0.1, 1, 1, 0.1, 10, 0.001, 1, 1))
rownames(parRanges) = c("q10", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10")


sensvars = c("PLANT.C", "PLANT.N", "LITTER.C", "LITTER.N", "SOM.C", "SOM.N",
             "NFIXMIC.C", "NFIXMIC.N", "NITROGEN")


#global sensitivity analysis

sR <- summary(sensRange(func=solvemodel, parms=params, dist ="unif", 
                sensvar = sensvars, parRange=parRanges[1,], num=50))

head(sR) #summary table
#plots 
plot(sR, xlab = "day", ylab = "g/m2", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")






