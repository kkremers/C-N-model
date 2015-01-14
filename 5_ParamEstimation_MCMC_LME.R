##################MCMC using FME package###########


#First, I am going to use fake data that is the output from the model with the parameters that I specified above
#doing this will allow us to know if the MCMC is picking the correct values for the model



#Get fake data ready
head(out)
data.assim = out[,c(1:8, 10,11)]
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
par(mfrow=c(3,2))
head(data.assim)
plot(data.assim$Biomass_C~data.assim$time, pch=16, ylab="Biomass_C", xlab="Time (days)")
plot(data.assim$Biomass_N~data.assim$time, pch=16, ylab="Biomass_N", xlab="Time (days)")
plot(data.assim[,4])
plot(data.assim[,5])
plot(data.assim[,6])
plot(data.assim[,7]
     plot(data.assim[,8])
     plot(data.assim[,9])
     plot(data.assim[,10])
     
     
     
     #let's start with noninformative priors
     #we don't need to specify a function for the priors because noninformative priors are the default for the functions used to fit the model and run the MCMC
     
     
     #First, create function that returns the "model cost" or the residuals
     
     cost <- function(params){
       modout = solvemodel(params)
       modout = data.frame(modout[,c(1:8, 11)]) #choose columns that you want
       modout1 = modout[match(data.assim$time, modout$time),] #only include time points for that have data
       return(modCost(modout1, data.assim, x="time"))
     }
     
     #Fit the model to the data
     Fit = modFit(p=params, f=cost, lower=rep(0, length=length(params)))
     sFit = summary(Fit) #summarize the fit
     
     mse = Fit$var_ms_unweighted #initial model variance for MCMC
     #Cov = sFit$cov.scaled  #proposal distribution (initial jumping width) for MCMC
     params.jump = params/1000  #proposal distribution (initial jumping width) for MCMC
     MCMC =  modMCMC(f=cost, p=Fit$par, jump=params.jump, updatecov = 100, 
                     lower=rep(0, length=length(params)), var0=mse, niter=50000) #adaptive metropolis
     
     MCMC2 =  modMCMC(f=cost, p=Fit$par, jump=params.jump, updatecov = 100, 
                      lower=rep(0, length=length(params)), var0=mse, niter=100000) #adaptive metropolis
     
     
     par(mfrow=c(3,2), mar=c(4,4,2,2))
     plot(MCMC2, Full=TRUE)
     
     #plot effects of estimated parameters on model output
     sR= sensRange(func=solvemodel, parms=params, sensvar = sensvars, parInput = MCMC$pars)
     sR.summ = summary(sR)
     #plots 
     par(mfrow=c(3,2)) 
     plot(sR.summ, xlab = "Time (days)")
     