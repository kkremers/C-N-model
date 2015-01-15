##################MCMC using FME package###########


#First, I am going to use fake data that is the output from the model with the parameters that I specified above
#doing this will allow us to know if the MCMC is picking the correct values for the model



#Get fake data ready
head(out)
data.assim = out[,c(1:8, 11, 10)]
head(data.assim)

#add some noise to the data
for (i in 1:1826){
  for (j in 2:length(data.assim)){
    data.assim[i,j] = data.assim[i,j] + rnorm(1, 0, abs((data.assim[i,j]/100)))
    
  } 
}

#remove some data points
time.keep  = seq(1, length(time), 15) #keep data for every 15 days
data.assim = data.assim[match(time.keep, data.assim$time),] 
head(data.assim)

#plot to view data
par(mfrow=c(4,2), mar=c(4,4,2,2))
head(data.assim)
plot(data.assim$Biomass_C~data.assim$time, xlab="Time (days)", ylab="Biomass_C (g/m2)", pch=16, col="springgreen3")
plot(data.assim$Biomass_N~data.assim$time, xlab="Time (days)", ylab="Biomass_N (g/m2)", pch=16, col="springgreen3")
plot(data.assim$Litter_C~data.assim$time, xlab="Time (days)", ylab="Litter_C (g/m2)", pch=16, col="chocolate2")
plot(data.assim$Litter_N~data.assim$time, xlab="Time (days)", ylab="Litter_N (g/m2)", pch=16, col="chocolate2")
plot(data.assim$SOM_C~data.assim$time, xlab="Time (days)", ylab="SOM_C (g/m2)", pch=16, col="darkred")
plot(data.assim$SOM_N~data.assim$time, xlab="Time (days)", ylab="SOM_N (g/m2)", pch=16, col="darkred")
plot(data.assim$Available_N~data.assim$time, xlab="Time (days)", ylab="Available_N (g/m2)", pch=16, col="green3")
#plot(data.assim$LAI~data.assim$time, xlab="Time (days)", ylab="LAI (g/m2)", pch=16, col="blue")
plot(data.assim$NEE~data.assim$time, xlab="Time (days)", ylab="NEE (g/m2)", pch=16, col="darkolivegreen4")


data.assim2 = data.assim[,c(1,9)]
head(data.assim2)

data.assim3 = data.assim[,c(1,9,10)]
head(data.assim3)

#let's start with noninformative priors
#we don't need to specify a function for the priors because noninformative priors are the default for the functions used to fit the model and run the MCMC

#First, create functions that returns the "model cost" or the residuals

cost <- function(params){
  modout = solvemodel(params)
  modout = data.frame(modout[,c(1:8,10,11)]) #choose columns that you want
  modout1 = modout[match(data.assim$time, modout$time),] #only include time points for that have data
  return(modCost(modout1, data.assim, x="time"))
}

cost2 <- function(params){
  modout = solvemodel(params)
  modout = data.frame(modout[,c(1,11)]) #choose columns that you want
  modout1 = modout[match(data.assim2$time, modout$time),] #only include time points for that have data
  return(modCost(modout1, data.assim2, x="time"))
}

cost3 <- function(params){
  modout = solvemodel(params)
  modout = data.frame(modout[,c(1,10,11)]) #choose columns that you want
  modout1 = modout[match(data.assim3$time, modout$time),] #only include time points for that have data
  return(modCost(modout1, data.assim3, x="time"))
}

#Fit the model to the data
Fit = modFit(p=params, f=cost, lower=rep(0, length=length(params)))
mse = Fit$var_ms_unweighted #initial model variance for MCMC

mse2 = mse[8]
mse3 = mse[c(9,8)]

params.jump = params/10000  #proposal distribution (initial jumping width) for MCMC
#MCMC =  modMCMC(f=cost, p=Fit$par, jump=params.jump, updatecov = 100, 
#               lower=rep(0, length=length(params)), var0=mse, niter=50000) #adaptive metropolis

MCMC2 =  modMCMC(f=cost, p=Fit$par, jump=params.jump, updatecov = 100, 
                 lower=rep(0, length=length(params)), var0=mse, niter=100000) #adaptive metropolis

#re-run this one if we have time
MCMC3 =  modMCMC(f=cost2, p=Fit$par, jump=params.jump, updatecov = 100, 
                 lower=rep(0, length=length(params)), var0=mse2, niter=100000) #adaptive metropolis

MCMC4 =  modMCMC(f=cost3, p=Fit$par, jump=params.jump, updatecov = 50, 
                 lower=rep(0, length=length(params)), var0=mse3, niter=100000) #adaptive metropolis


#Trace Plots
par(mfrow=c(3,3), mar=c(4,4,2,2))
plot(MCMC2, Full=TRUE)
plot(MCMC3, Full=TRUE)
plot(MCMC4, Full=TRUE)

MCMC2.iters=data.frame(MCMC2$pars[90001:100000,]) #keep last 10,000
MCMC3.iters=data.frame(MCMC3$pars[90001:100000,])
MCMC4.iters=data.frame(MCMC4$pars[50001:60000,])

###PLOTS OF DISTRIBUTIONS

par(mfrow=c(4,2), mar=c(4,4,2,2))
#kplant
plot(density(MCMC2.iters$kplant), main="kplant", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$kplant), main="kplant", xlab="", ylab="", col="blue", xlim=c(0, 6))
abline(density(MCMC4.iters$kplant), col="darkolivegreen4")
abline(v=0.11, col="red", lwd=3) #add line where actual value should be

#LitterRate
plot(density(MCMC2.iters$LitterRate), main="LitterRate", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$LitterRate), main="LitterRate", xlab="", ylab="", col="blue")
abline(density(MCMC4.iters$LitterRate), col="darkolivegreen4")
abline(v=0.0015, col="red", lwd=3) #add line where actual value should be

#DecompRateC
plot(density(MCMC2.iters$DecompRateC), main="DecompRateC", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$DecompRateC), col="blue", main="DecompRateC", xlab="", ylab="")
abline(density(MCMC4.iters$DecompRateC), col="darkolivegreen4")
abline(v=0.005, col="red", lwd=3) #add line where actual value should be

#DecompRateN
plot(density(MCMC2.iters$DecompRateN), main="DecompRateN", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$DecompRateN), col="blue", main="DecompRateN", xlab="", ylab="")
abline(density(MCMC4.iters$DecompRateN), col="darkolivegreen4")
abline(v=0.0007, col="red", lwd=3) #add line where actual value should be

#retrans
plot(density(MCMC2.iters$retrans), main="retrans", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$retrans), col="blue", main="retrans", xlab="", ylab="")
abline(density(MCMC4.iters$retrans), col="darkolivegreen4")
abline(v=0.7, col="red", lwd=3) #add line where actual value should be

#RespRateSOM
plot(density(MCMC2.iters$RespRateSOM), main="RespRateSOM", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$RespRateSOM), col="blue", main="RespRateSOM", xlab="", ylab="", xlim=c(0,0.00035))
abline(density(MCMC4.iters$RespRateSOM), col="darkolivegreen4")
abline(v=0.00001, col="red", lwd=3) #add line where actual value should be

#PropResp
plot(density(MCMC2.iters$PropResp), main="PropResp", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$PropResp), col="blue", main="PropResp", xlab="", ylab="")
abline(density(MCMC4.iters$PropResp), col="darkolivegreen4")
abline(v=0.5, col="red", lwd=3) #add line where actual value should be

#kCUE
plot(density(MCMC2.iters$kCUE), main="kCUE", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$kCUE), col="blue", main="kCUE", xlab="", ylab="")
abline(density(MCMC4.iters$kCUE), col="darkolivegreen4")
abline(v=0.004, col="red", lwd=3) #add line where actual value should be

#UptakeRate
plot(density(MCMC2.iters$UptakeRate), main="UptakeRate", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$UptakeRate), col="blue", main="UptakeRate", xlab="", ylab="")
abline(density(MCMC4.iters$UptakeRate), col="darkolivegreen4")
abline(v=0.0001, col="red", lwd=3) #add line where actual value should be

#netNrate
plot(density(MCMC2.iters$netNrate), main="netNrate", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$netNrate), col="blue", main="netNrate", xlab="", ylab="")
abline(density(MCMC4.iters$netNrate), col="darkolivegreen4")
abline(v=0.0008, col="red", lwd=3) #add line where actual value should be

#Biomass_C
plot(density(MCMC2.iters$Biomass_C), main="Biomass_C", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$Biomass_C), col="blue", main="Biomass_C", xlab="", ylab="")
abline(density(MCMC4.iters$Biomass_C), col="darkolivegreen4")
abline(v=400, col="red", lwd=3) #add line where actual value should be

#Biomass_N
plot(density(MCMC2.iters$Biomass_N), main="Biomass_N", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$Biomass_N), col="blue", main="Biomass_N", xlab="", ylab="")
abline(density(MCMC4.iters$Biomass_N), col="darkolivegreen4")
abline(v=4.75, col="red", lwd=3) #add line where actual value should be

#Litter_C
plot(density(MCMC2.iters$Litter_C), main="Litter_C", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$Litter_C), col="blue", main="Litter_C", xlab="", ylab="")
abline(density(MCMC4.iters$Litter_C), col="darkolivegreen4")
abline(v=100, col="red", lwd=3) #add line where actual value should be

#Litter_N
plot(density(MCMC2.iters$Litter_N), main="Litter_N", xlab="", ylab="") #plot distribution
plot(density(MCMC3.iters$Litter_N), col="blue", main="Litter_N", xlab="", ylab="")
abline(density(MCMC4.iters$Litter_N, col="darkolivegreen4")
       abline(v=1.6, col="red", lwd=3) #add line where actual value should be
       
       #SOM_C
       plot(density(MCMC2.iters$SOM_C), main="SOM_C", xlab="", ylab="") #plot distribution
       plot(density(MCMC3.iters$SOM_C), col="blue", main="SOM_C", xlab="", ylab="", xlim=c(-50, 2100))
       abline(density(MCMC4.iters$SOM_C), col="darkolivegreen4")
       abline(v=2000, col="red", lwd=3) #add line where actual value should be
       
       #SOM_N
       plot(density(MCMC2.iters$SOM_N), main="SOM_N", xlab="", ylab="") #plot distribution
       plot(density(MCMC3.iters$SOM_N), col="blue", main="SOM_N", xlab="", ylab="")
       abline(density(MCMC4.iters$SOM_N), col="darkolivegreen4")
       abline(v=56, col="red", lwd=3) #add line where actual value should be
       
       #Available_N
       plot(density(MCMC2.iters$Available_N), main="Available_N", xlab="", ylab="") #plot distribution
       plot(density(MCMC3.iters$Available_N), col="blue", main="Available_N", xlab="", ylab="")
       abline(density(MCMC4.iters$Available_N), col="darkolivegreen4")
       abline(v=0.1, col="red", lwd=3) #add line where actual value should be
       
       
       
       
       #Plot effects of estimated parameters on model output
       sR2= sensRange(func=solvemodel, parms=params, sensvar = sensvars, parInput = MCMC2$pars[90001:100000,])
       sR2.summ = summary(sR2)
       
       sR3= sensRange(func=solvemodel, parms=params, sensvar = sensvars, parInput = MCMC3$pars[90001:100000,])
       sR3.summ = summary(sR3)
       
       sR4= sensRange(func=solvemodel, parms=params, sensvar = sensvars, parInput = MCMC4$pars[50001:60000,])
       sR4.summ = summary(sR4)
       
       #PLOTS OF GLOBAL SENSITIVITY ANALYSIS
       par(mfrow=c(3,2), mar=c(4,4,2,2))
       plot(sR2.summ, xlab = "Time (days)", mfrow = NULL,
            quant = TRUE, col = c("darkslategray3", "darkslategray"), legpos = "topright")
       
       par(mfrow=c(3,2), mar=c(4,4,2,2))
       plot(sR3.summ, xlab = "Time (days)", mfrow = NULL, 
            quant = TRUE, col = c("darkslategray3", "darkslategray"), legpos = "topright")
       
       par(mfrow=c(3,2), mar=c(4,4,2,2))
       plot(sR4.summ, xlab = "Time (days)" mfrow = NULL,
            quant = TRUE, col = c("darkslategray3", "darkslategray"), legpos = "topright")