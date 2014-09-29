
library(deSolve)


numyears = 4
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


#parameters
q10 = 2 #q10
q10p = 8 #sistla paper
t1 = 0.05 #Rh constant
t2 = 0.007 #Litter rate constant; Hobbie 2002
t3 = 0.03 #Decomp rate constant



Temp.d1 <- approxfun(x=time, y=Temp, method="linear", rule=2)
GPP.d1 <- approxfun(x=time, y=GPP, method="linear", rule=2)



model<-function(t,state,parms)
{
  with(as.list(c(state)),{ #start of with(as.list(...
    
    Temp=Temp.d1(t)
    GPP=GPP.d1(t)
    
    
    #FLUXES
    s=1/(1+exp(-Temp))
    U = (5*(PLANT.C)*(NITROGEN/(0.05+NITROGEN)))*s
    k=0.01*exp(-0.01*U)
    cue=(U/(k+U))/1.5
    NPP = cue*GPP #cue*GPP
    Ra = (1-cue)*GPP
    
    
    L.C = t2*PLANT.C
    L.N = t2*PLANT.N
    
    D.C = t3*LITTER.C*q10^(Temp/10)
    D.N = t3*LITTER.N*q10^(Temp/10)
    
    Mic.C = 0.2*NFIXMIC.C*(SOM.C/(0.1+SOM.C))*s
    Mic.N = 0.05*NFIXMIC.N*(SOM.N/(0.1+SOM.N))*s
    A = 0.8*Mic.N
  
    Min = 0.0018*(q10^(Temp/10)) #gN/m2*day mineralization rate per day; Hobbie 2002
    Nfix.ml = 0.004*(q10^(Temp/10)) #gN/m2*day #rate of N fixed by mosses and lichens; Porada 2013; could be conservative estimate
    Nfix.f = 0.0025*(q10^(Temp/10)) #gN/m2*day rate of N fixed by fungi/mycorrhizae; Hobbie 2006
    death.C=0.003*NFIXMIC.C
    death.N=0.003*NFIXMIC.N    
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
           
    ),
    c(Temp=Temp.d1(t),
      GPP=GPP.d1(t),
      NPP=NPP,
      Ra=Ra,
      Rh=Rh,
      Re=Re,
      U=U,
      L.C=L.C,
      L.N=L.N,
      D.C=D.C,
      D.N=D.N,
      Mic.N=Mic.N,
      Mic.C=Mic.C,
      death.C=death.C,
      death.N=death.N,
      Min=Min,
      Nfix.ml=Nfix.ml,
      Ndep=Ndep,
      A=A,
      s=s,
      Nfix.f=Nfix.f,
      cue = cue
    ))
    
  })  #end of with(as.list(...
} #end of model



TimeFrom = 1
TimeEnd = TIME

Time = TimeFrom
out = NULL


while (Time < TimeEnd)
{
  times=seq(TimeFrom, TimeEnd, 1) 
  if (length(times)>1) {
    out1    <- as.data.frame(ode(y=state,times=times,func=model,parms=0)) #integrate
    out     <- rbind(out,out1)
    
    lout    <- nrow(out1)          #last element of output
    
    state   <-c(PLANT.C= out1[lout, "PLANT.C"], 
                PLANT.N= out1[lout, "PLANT.N"],
                SOM.C= out1[lout, "SOM.C"],
                SOM.N= out1[lout, "SOM.N"],
                LITTER.C= out1[lout, "LITTER.C"], 
                LITTER.N= out1[lout, "LITTER.N"],
                NFIXMIC.C=out1[lout, "NFIXMIC.C"],
                NFIXMIC.N=out1[lout, "NFIXMIC.N"],
                NITROGEN= out1[lout, "NITROGEN"]
                
    )    
    
    Time <- TimeEnd #RESET TIME, STATE VARIABLES
  }
}





#PLOTS for presentation
#Forcing data
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(GPP)
plot(Temp, type="l")
plot(PAR, type="l")
plot(LAI, type="l")


#change in stocks
par(mfrow=c(4,2),mar=c(4,4,2,2))
plot(out$PLANT.C~time, type="l", col="green", lwd=2)
plot(out$PLANT.N~time, type="l", col="green", lwd=2)
plot(out$LITTER.C~time, type="l", col="blue", lwd=2)
plot(out$LITTER.N~time, type="l", col="blue", lwd=2)
plot(out$SOM.C~time, col="red", type="l", lwd=2)
plot(out$SOM.N~time, col="red", type="l", lwd=2)
plot(out$NFIXMIC.C~time, col="purple", type="l", lwd=2)
plot(out$NFIXMIC.N~time, col="purple", type="l", lwd=2)

plot(out$NITROGEN~time, col="black", type="l", lwd=2)

plot(out$cue~out$U)
plot(out$s~out$Temp)
plot(out$U~time, type="l",  col="black", lwd=2)
plot(out$Nfix.ml~time, type="l",  col="black", lwd=2)
plot(out$Nfix.f~time, type="l",  col="black", lwd=2)
plot(out$death.N~time, type="l",  col="black", lwd=2)
plot(out$Min~time, type="l",  col="black", lwd=2)
plot(out$Mic.C~time, type="l",  col="black", lwd=2)
plot(out$A~time, type="l",  col="black", lwd=2)

#Carbon fluxes
plot(out$GPP~time, type="l", main="Carbon Fluxes", ylim=c(-20, 15))
lines(out$NPP~time, col="green")
lines(-out$Re~time, col="purple")
abline(h=0, lty=2)

plot((out$GPP-out$Re)~time, type="l", main="Carbon Fluxes", ylim=c(-20, 15))
abline(h=0, lty=2)

plot(-out$Re~time, type="l", main="Respiration Fluxes", col="black")
lines(-out$Rh~time, col="red")
lines(-out$Ra~time, col="blue")
abline(h=0, lty=2)

par(mfrow=c(1,3),mar=c(4,4,2,2))
plot(out$Rh~out$Temp, col="red")
plot(out$Ra~out$Temp, col="blue")
plot(out$Re~out$Temp, col="purple")


