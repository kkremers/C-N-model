
library(deSolve) #you may need to install this package first


numyears = 5 #number of years to run model for
TIME=365*numyears #runs on a daily timestep
time=seq(1,TIME,1) #creates time sequence


#SIMULATING ENVIRONMENTAL DATA
#for running the model initially, I am similating temperature and PAR data to be similar to an average year, with some noise
#when we do real model runs, we can use real data for whatever site and years we decide to run the model for
gaussian <- function(x, a, b, c) {  #x is data, a is height, b is peak position (mean), c is width (standard deviation)
  a * exp(-((x-b)^2)/(2*(c^2))) }

Temp=gaussian(1:365, 40, 195, 70)-20
Temp=rep.int(Temp, numyears)

PAR=gaussian(1:365, 600, 170, 60)
PAR=rep.int(PAR, numyears)

#make noisy
for (i in 1:TIME){
  Temp[i]=Temp[i]+rnorm(1,0,2)}


for (i in 1:TIME){
  PAR[i]=PAR[i]+rnorm(1,0,15)
  if(PAR[i]<0){PAR[i]=0}}


set.seed(1)


par(mfrow=c(1,2)) #plot the simulated data
plot(PAR, type="l")
plot(Temp, type="l")



#STATE VARIABLES
C.f = 200 
N.f = 6 #C:N should be ~30; Chapin and Shaver 1988, Sistla et al. 2014
C.w = 200
N.w = 2 #C:N should be ~100; Chapin and Shaver 1988, Sistla et al. 2014
C.r = 180
N.r = 3 #C:N should be ~60; Chapin and Shaver 1988, Sistla et al. 2014
C.lit = 80
N.lit = 2
C.som = 2000
N.som = 55 #C:N should be ~36.5; Moorehead and Reynolds 1993
C.mic = 90
N.mic = 9 #C:N should be ~9, but depends on community type; Moorehead and Reynolds 1993

state = c(C.f = C.f,
            N.f = N.f,
            C.w = C.w,
            N.w = N.w,
            C.r = C.r,
            N.r = N.r,
            C.lit = C.lit,
            N.lit = N.lit,
            C.som = C.som,
            N.som = N.som,
            C.mic = C.mic,
            N.mic = N.mic)


#parameters
theta1=0.33    # /day; allocation of C and N to foliage; I am assuming it is split up relatively evenly between the three plant pools
theta2=0.33  # /day; allocation of C and N to wood
theta3=0.3 # /day; nitrogen decomposition rate
theta4=0.2   # /day; carbon decomposition rate
theta5=0.03   # /day; respiration rate constant for RH1
theta6=0.04   # /day; respiration rate constant for RH2
theta7=0.01   # /day; respiration rate constant for RH3
theta8=0.01285   # /day; microbial carbon uptake constant
theta9=0.1    #N and C available to microbes
theta10=0.00175  # /day; microbial N uptake constant
theta11=0.004 # /day; death rate
theta13=0.0005 # /day; mineralization rate #Shmidt 1999
theta14=0.000025     # plant N uptake constant
theta15=0.05  # N available to plants
theta16=0.01 #/day; litter rate of foliage; Hobbie 1996, Sistla et al. 2014
theta17=0.01 #/day; litter rate of wood; Hobbie 1996, Sistla et al. 2014
theta18=0.01 #/day; litter rate of roots; Hobbie 1996, Sistla et al. 2014
q10=2       #microbial Q10; Williams 2005
q10p=8      #plant Q10 #sistla 2014
pmax = 7.999 #umol CO2/gN*s; maximum photosynthetic rate
E0 = 0.029  #umolCO2/umolPAR;light use efficiency
k = 0.5 #m2/gN; Beer's law extinction coefficient
cue = 0.5 #carbon use efficiency



Temp.d1 <- approxfun(x=time, y=Temp, method="linear", rule=2) 
PAR.d1  <- approxfun(x=time, y=PAR, method="linear", rule=2)

model<-function(t,state,parms)
{
  with(as.list(c(state)),{ #start of with(as.list(...
    
    Temp=Temp.d1(t)
    PAR=PAR.d1(t)
    
    #FLUXES
    Af.N = theta1*(theta14*(C.f+C.r+C.w)*(N.som/(theta15+N.som))*q10p^(Temp/10))
    Aw.N = theta2*(theta14*(C.f+C.r+C.w)*(N.som/(theta15+N.som))*q10p^(Temp/10))
    Ar.N = (1-theta1-theta2)*(theta14*(C.f+C.r+C.w)*(N.som/(theta15+N.som))*q10p^(Temp/10))
    GPP = (pmax/k)*log((pmax+E0*PAR)/(pmax+E0*PAR*exp(-k*(N.f))))*(10^-6)*12*86400
    
    NPP = cue*GPP #cue*GPP
    Ra = (1-cue)*GPP
    Af.C = theta1*NPP
    Aw.C = theta2*NPP
    Ar.C = (1-theta1-theta2)*NPP
    Lf.C = theta16*C.f
    Lf.N = theta16*N.f
    Lw.C = theta17*C.w
    Lw.N = theta17*N.w
    Lr.C = theta18*C.r
    Lr.N = theta18*N.r
    D.C = theta4*C.lit*q10^(Temp/10)
    D.N = theta3*N.lit*q10^(Temp/10)
    Umic.C = theta8*C.mic*(C.som/(theta9+C.som))*q10^(Temp/10)
    Umic.N = theta10*C.mic*(N.som/(theta9+N.som))*q10^(Temp/10)
    death.C=theta11*C.mic
    death.N=theta11*N.mic  
    Min = theta13*N.mic*(q10^(Temp/10)) #gN/m2*day mineralization rate per day; Hobbie 2002
    Rh1 = theta5*C.lit*q10^(Temp/10)
    Rh2 = theta6*C.som*q10^(Temp/10)
    Rh3 = theta7*C.mic*q10^(Temp/10)
    Re= Ra+Rh1+Rh2+Rh3
    
    
    #differential equations
    dC.f = Af.C-Lf.C
    dN.f = Af.N-Lf.N
    dC.w = Aw.C - Lw.C
    dN.w = Aw.N - Lw.N
    dC.r = Ar.C-Lr.C
    dN.r = Ar.N-Lr.N
    dC.lit = Lf.C+Lr.C+Lw.C-D.C-Rh1
    dN.lit = Lf.N+Lr.N+Lw.C-D.N
    dC.som = D.C+death.C-Umic.C-Rh2
    dN.som = D.N+Min+death.N-Umic.N-Af.N-Ar.N-Aw.N
    dC.mic = Umic.C-Rh3-death.C
    dN.mic = Umic.N-Min-death.N
    
    
    #what to output
    
    list(c(dC.f, dN.f, dC.w, dN.w, dC.r, dN.r, dC.lit, dN.lit, dC.som, dN.som, dC.mic, dN.mic),
    c(Temp=Temp.d1(t),
      PAR=PAR.d1(t),
      GPP=GPP,
      NPP=NPP,
      Ra=Ra,
      Rh1=Rh1,
      Rh2=Rh2,
      Rh3=Rh3,
      Re=Re,
      Af.C=Af.C,
      Af.N=Af.N,
      Aw.C=Aw.C,
      Aw.N=Aw.N,
      Ar.C=Ar.C,
      Ar.N=Ar.N,
      Lf.C=Lf.C,
      Lf.N=Lf.N,
      Lr.C=Lr.C,
      Lr.N=Lr.N,
      D.C=D.C,
      D.N=D.N,
      Umic.N=Umic.N,
      Umic.C=Umic.C,
      death.C=death.C,
      death.N=death.N,
      Min=Min,
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
    
    state   <-c(C.f= out1[lout, "C.f"], 
                N.f= out1[lout, "N.f"],
                C.r= out1[lout, "C.r"], 
                N.r= out1[lout, "N.r"],
                C.som= out1[lout, "C.som"],
                N.som= out1[lout, "N.som"],
                C.lit= out1[lout, "C.lit"], 
                N.lit= out1[lout, "N.lit"],
                C.mic=out1[lout, "C.mic"],
                N.mic=out1[lout, "N.mic"]                
    )    
    
    Time <- TimeEnd #RESET TIME, STATE VARIABLES
  }
}



#PLOTS for presentation
#Forcing data
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(out$GPP, type="l")
plot(Temp, type="l")
plot(PAR, type="l")



#change in stocks
par(mfrow=c(2,2),mar=c(4,4,2,2))
plot(out$C.f~time, type="l", col="green", lwd=2)
plot(out$N.f~time, type="l", col="green", lwd=2)
plot(out$C.w~time, type="l", col="green", lwd=2)
plot(out$N.w~time, type="l", col="green", lwd=2)
plot(out$C.r~time, type="l", col="green", lwd=2)
plot(out$N.r~time, type="l", col="green", lwd=2)
plot(out$C.lit~time, type="l", col="blue", lwd=2)
plot(out$N.lit~time, type="l", col="blue", lwd=2)
plot(out$C.som~time, col="red", type="l", lwd=2)
plot(out$N.som~time, col="red", type="l", lwd=2)
plot(out$C.mic~time, col="purple", type="l", lwd=2)
plot(out$N.mic~time, col="purple", type="l", lwd=2)


#C:N ratios
plot((out$C.f/out$N.f)~time, col="green", type="l", lwd=2)
plot((out$C.r/out$N.r)~time, col="green", type="l", lwd=2)
plot((out$C.lit/out$N.lit)~time, col="green", type="l", lwd=2)
plot((out$C.som/out$N.som)~time, col="green", type="l", lwd=2)
plot((out$C.mic/out$N.mic)~time, col="green", type="l", lwd=2)

#Carbon fluxes
plot(out$GPP~time, type="l", main="Carbon Fluxes")
lines(out$NPP~time, col="green")
lines(-out$Re~time, col="purple")
abline(h=0, lty=2)

plot((out$GPP-out$Re)~time, type="l", main="Carbon Fluxes")
abline(h=0, lty=2)

plot(-out$Re~time, type="l", main="Respiration Fluxes", col="black")
lines(-(out$Rh1+out$Rh2+out$Rh3)~time, col="red")
lines(-out$Ra~time, col="blue")
abline(h=0, lty=2)
plot(out$Min~time, type="l")
plot(out$Umic.N~time, type="l")
