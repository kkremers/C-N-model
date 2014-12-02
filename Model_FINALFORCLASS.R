####Load packages
require(deSolve)
require(FME)


#
####################################

#using real data

#first upload and viewdata 

data = read.csv("OriginalData_NOTfilled.csv") #choose file from directory
names(data) = c("time", "year", "DOY", "Albedo", "Temp_T", "PAR_T", "Temp_ARF", "PAR_ARF", "EVI", "NDVI", "LAI", "NEE", "Re", "GPP")
head(data) #view table

#plot data
par(mfrow=c(2,1)) 
plot(data$Temp_T~data$time, type="l", ylab = "Daily Max Temp (C)", xlab="Time (days)") 
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16)
plot(data$PAR_T~data$time, type="l", ylab = "Daily Max PAR (umol m-2 s-1)", xlab="Time (days)")
points(data$PAR_ARF~data$time, col="blue", pch=16)
#ARF data is missing winter measurements - want to fill this in

#want to plot to determine relationship, and then fill in missing data in ARF data
#start with temperature
linmod.t = lm(data$Temp_ARF~data$Temp_T + 0) #linear model, set intercept = 0
summary(linmod.t) #summary
slope.t = summary(linmod.t)$coefficients[1,1] #slope value

par(mfrow=c(1,1))
plot(data$Temp_ARF~data$Temp_T, ylab = "ARF", xlab="Toolik", main="Temperature", pch=16, col="red")
abline(linmod.t, lwd = 2) #linear regression line

#do the same thing for PAR data
linmod.p = lm(data$PAR_ARF~data$PAR_T + 0)
summary(linmod.p)
slope.p = summary(linmod.p)$coefficients[1,1]

par(mfrow=c(1,1))
plot(data$PAR_ARF~data$PAR_T, ylab = "ARF", xlab="Toolik", main="PAR", pch=16, col="blue")
abline(linmod.p, lwd = 2)

#fill in missing data - NEED TO FIGURE OUT HOW TO CARRY UNCERTAINTY THROUGH THIS???

for(i in 1:length(data$Temp_ARF)){
  
  if(is.na(data$Temp_ARF[i])==TRUE) {
    data$Temp_ARF[i] = data$Temp_T[i]*slope.t 
  }
  
  if(is.na(data$PAR_ARF[i])==TRUE) {
    data$PAR_ARF[i] = data$PAR_T[i]*slope.p
  }
}

#need to convert units of PAR
data$PAR_ARF = data$PAR_ARF*(1E-6)*86400
data$PAR_T = data$PAR_T*(1E-6)*86400

#check output to make sure it all lines up
par(mfrow=c(2,1))
plot(data$Temp_T~data$time, type="l", ylab = "Daily Max Temp (C)", xlab="Time (days)")
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16)
plot(data$PAR_T~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", xlab="Time (days)")
points(data$PAR_ARF~data$time, col="blue", pch=16)

#Now we want to filter so that we only use the PAR that the plants are exposed to
#To do this, we will use albedo to create a new vector, PAR_vis, that only includes PAR data for snow-free days

PAR_vis = NULL

for(i in 1:length(data$PAR_ARF)){
  
  if(data$Albedo[i] > 0.2 ) { #if albedo is greater than 0.2
    PAR_vis[i] = 0
  }
  else {
    PAR_vis[i] = data$PAR_ARF[i]
  }
  
}

data = data.frame(data[,1:8], PAR_vis = PAR_vis, data[,9:14])
head(data)


#check output
par(mfrow=c(2,1))
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", main="All PAR", xlab="Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Max PAR (mol m-2 s-1)", main="Plant Available PAR", xlab="Time (days)")


write.csv(data, "FluxData.csv") #added the updated data to the working directory so that it is easy to access - won't have to do any of the above steps again
##################################################

############re-loading the data

#set working directory to C-N-model
data = read.csv("InputData_Processed.csv") #This is the "FluxData.csv" file, but with added calculations of GDD
head(data)

#plot the data
par(mfrow=c(2,2), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$delGDD~data$time, type="l", ylab = "delGDD (change in degrees C /day)",  xlab="", col="forestgreen")
plot(data$PAR_ARF~data$time, type="l", ylab = "Daily PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

#need to figure out which DOY was the maximum delGDD for each year
years = unique(data$year) #tells you which years we have data for 
DOY.sen = NA
for (i in 1: length(years)){
  year.i = years[i]
  data.year = subset(data, data$year==year.i)
  delGDDmax.day = data.year$DOY[which(data.year$delGDD == max(data.year$delGDD))]
  DOY.s = rep(delGDDmax.day, length=length(data.year[,1]))
  DOY.sen = c(DOY.sen, DOY.s)
}

DOY.sen = DOY.sen[2:length(DOY.sen)]

data = data.frame(data, DOY.sen = DOY.sen)

#make into functions so that it will be continuous in the model
Temp.d1 <- approxfun(x=data$time, y=data$Temp_ARF, method="linear", rule=2)
PAR.d1 <- approxfun(x=data$time, y=data$PAR_vis, method="linear", rule=2)
delGDD.d1 <-approxfun(x=data$time, y=data$delGDD, method="linear", rule=2)
DOY.d1 <- approxfun(x=data$time, y=data$DOY, method="linear", rule=2)
DOYsen.d1 <- approxfun(x=data$time, y=data$DOY.sen, method="linear", rule=2)

######################Parameters and initial state variables##########################
params <- c(LitterRate = 0.0012,
            DecompRate = 0.001, 
            retrans = 0.8,  
            RespRate = 1E-4, 
            PropResp = 0.5,
            kCUE = 0.001,
            kplant = 15,
            UptakeRate = 5E-5,
            Biomass_C = 900, 
            Biomass_N = 15, 
            Litter_C = 500, 
            Litter_N = 7, 
            SOM_C = 2000, 
            SOM_N = 56)


####################MODEL#################################
time = seq(1, 1826, 1)

solvemodel <- function(params, times=time) {
  
  model<-function(t,state,params)
  { 
    with(as.list(c(state, params)),{ #start of with(as.list(...
      
      #forcing data
      Temp=Temp.d1(t)
      PAR=PAR.d1(t)
      delGDD = delGDD.d1(t)
      DOY = DOY.d1(t)
      DOY.sen = DOYsen.d1(t)
      delGDD.max = max(data$delGDD) 
      delGDD.min = min(data$delGDD)
      
      
      #constants for PLIRTLE model - Loranty 2011 - will not try to estimate these
      k=0.63
      Pmax = 1.18
      E0 = 0.03
      q10 = 2
      LAC = 0.012 #calculated from LTER data
      qSOM = 35.7 #g C / g N ; Moorehead and Reynolds 1993
      CUEmax = 0.9
      
      #FLUXES
      s.GDD = (delGDD - delGDD.min)/(delGDD.max-delGDD.min) #growing degree day scalar
      LAI = (Biomass_C*0.25)*LAC*s.GDD
      GPP = ( Pmax / k ) * log ( ( Pmax + E0 * PAR ) / ( Pmax + E0 * PAR * exp ( - k * LAI ) ) ) * 12 
      Uptake =  UptakeRate * (Biomass_C*0.5) * ( SOM_N / ( kplant + SOM_N ) ) * s.GDD
      cue = CUEmax * (Uptake/(kCUE + Uptake))
      Ra =  ( 1 - cue ) * GPP
      Re = RespRate * (q10 ^ ( ( Temp - 10 )/ 10 ) )
      Rh = Re - Ra
      Rh1 =  PropResp * Rh
      Rh2 =  (1-PropResp) * Rh
      Decomp_C =  DecompRate * Litter_C * ( q10 ^ ( Temp / 10 ) )
      Decomp_N =  DecompRate * Litter_N * ( q10 ^ ( Temp / 10 ) )
      
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
      dLitter_C = Litterfall_C  - Rh1  - Decomp_C 
      dLitter_N = Litterfall_N  - Decomp_N 
      dSOM_C = Decomp_C  - Rh2 
      dSOM_N = Decomp_N  + N_dep - Uptake
      
      
      #what to output
      
      list(c(dBiomass_C, 
             dBiomass_N, 
             dLitter_C, 
             dLitter_N, 
             dSOM_C, 
             dSOM_N),
           c(GPP=GPP, LAI=LAI, NEE=NEE, Re=Re, 
             cue=cue, Ra=Ra, Rh1=Rh1, Rh2=Rh2, 
             Uptake = Uptake, s.GDD=s.GDD))
      
    })  #end of with(as.list(...
  } #end of model
  
  
  return(ode(y=params[9:14],times=time,func=model,parms = params[1:8], method="rk4")) #integrate using runge-kutta 4 method
  
} #end of solve model

#####################################################################

out = data.frame(solvemodel(params)) #creates table of model output

##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")



#plot pools
par(mfrow=c(3,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="springgreen3", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="springgreen3",  main = "Biomass N", xlab="", ylab="g N m-2", lty=2)
plot(out$Litter_C~out$time, type="l", col="orange", main = "Litter C", xlab="", ylab="g C m-2")
plot(out$Litter_N~out$time, type="l", col="orange", main = "Litter N", xlab="", ylab="g N m-2", lty=2)
plot(out$SOM_C~out$time, type="l", col="red", main = "SOM C", xlab="Time (days)", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="red", main = "SOM N", xlab="Time (days)", ylab="g N m-2",lty=2)



#see how well data matches
#to compare on 1:1 line with data, need to select only points for which data is available
data.compare=read.csv("ALLData_Assim.csv")
data.compare=data.compare[,1:5]
data.compare=data.compare[complete.cases(data.compare),]
head(data.compare)
out.compare = out[match(data.compare$time, out$time),]

par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$GPP~out$time, col="forestgreen", pch=18, ylim=c(0,6), main="GPP", ylab="Flux (gC m-2 day-1)", xlab="")
points(data$GPP, col="blue", pch=16, cex=0.6)
plot(data.compare$GPP, out.compare$GPP)
abline(0,1, col="red")

plot(out$LAI~out$time, col="orange", pch=18, ylim=c(0,1), main="LAI", ylab="LAI (m2 leaf m-2 ground)", xlab="" )
points(data$LAI, col="blue", pch=16, cex=0.6)
plot(data.compare$LAI, out.compare$LAI)
abline(0,1, col="red")

plot(-out$Re~out$time, col="red", pch=16, ylim=c(-5,0), main="Re", xlab="Time (days)", ylab="Flux (gC m-2 day-1)")
points(-data$Re, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare$Re, out.compare$Re)
abline(0,1, col="red")

plot(out$NEE~out$time, col="olivedrab3", pch=18, ylim=c(-3,2), main="NEE", xlab="Time (days)", ylab="Flux (gC m-2 day-1)")
points(data$NEE, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare$NEE, out.compare$NEE, ylim=c(-4, 1))
abline(0,1, col="red")


par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data$GPP~data$PAR_vis, pch=16, ylab="GPP", xlab="PAR_vis")
points(out$GPP~data$PAR_vis, col="red")

plot(data$LAI~data$Temp_ARF, pch=16, ylab="LAI", xlab="Temperature")
points(out$LAI~data$Temp_ARF, col="red")

plot(data$Re~data$Temp_ARF, pch=16, ylab="Re", xlab="Temperature")
points(out$Re~data$Temp_ARF, col="red")

plot(data$NEE~data$Temp_ARF, pch=16, ylab="NEE", xlab="Temperature")
points(out$NEE~data$Temp_ARF, col="red")




#plot CUE and LAI
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$cue~out$Uptake, xlab = "Uptake (g N m-2 day-1)", ylab = "Carbon Use Efficiency (CUE)")
plot(out$cue~out$time, type="l",  xlab = "Time (days)", ylab = "Carbon Use Efficiency (CUE)")
plot(out$Uptake~out$Available_N, xlab = "Available N (g N m-2)", ylab = "Uptake (g N m-2 day-1)")
plot(out$Uptake~out$time, type="l",  xlab = "Time (days)", ylab = "Uptake (g N m-2 day-1)")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$s.GDD~data$delGDD, xlab = "delGDD", ylab = "Scalar (s.GDD)")
plot(out$LAI~data$delGDD, xlab = "delGDD", ylab = "LAI (m2 m-2)")
plot(out$LAI~out$Biomass_C, xlab = "Biomass_C (gC m-2)", ylab = "LAI (m2 m-2)")


############SENSITIVITY ANALYSIS USING LME PACKAGE###############

sensvars = c("Biomass_C", 
             "Biomass_N", 
             "Litter_C", 
             "Litter_N", 
             "SOM_C", 
             "SOM_N",
             "GPP",
             "NEE",
             "Re",
             "LAI")


#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=params, senspar = names(params), 
                   sensvar = sensvars, varscale = 1)

head(s.local)
s.local.summ = summary(s.local, var=T)
s.loc.summ.ordered = s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] 
write.csv(s.loc.summ.ordered, "c:/Users/Rocha Lab/Desktop/Kelsey/LocalSensitivityAnalysis.csv") #univariate sensitivity
param.cor = data.frame(cor(s.local[,c(-1,-2)]))#table of parameter correlations
param.cor
write.csv(param.cor, "c:/Users/Rocha Lab/Desktop/Kelsey/ParamCorr.csv") #bivariate sensitivity
pairs(s.local)


#global sensitivity analysis

#alter all params by 20%
parms = as.vector(unlist(params))
paramsperc =parms*0.5
params.min =  parms - paramsperc
params.max = parms + paramsperc
parRanges = data.frame(min = params.min,  max = params.max)
rownames(parRanges) = names(params)
parRanges

s.global <- sensRange(func=solvemodel, parms=params, dist ="unif", 
                      sensvar = sensvars, parRange=parRanges, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "Time (days)", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


##############IDENTIFY PARAMS THAT CAN BE ESTIMATED###########

#this code is currently a little wonky - need to work on it

#load data to assimilate

data.assim = read.csv("ALLData_Assim.csv") #load data
head(data.assim)
data.assim = data.assim[,c(1,3:12)] #pull out columns that you need
head(data.assim)
#data.assim = data.assim[complete.cases(data.assim),] #only complete cases
#head(data.assim)
#create a function that returns the residuals of the model vs. the data (estimated by "modCost")
Objective <- function(x, parset=names(x)){ 
  params[parset] <- x
  out <- data.frame(solvemodel(params, state))
  return(modCost(obs = data.assim, model=out[match(data.assim$time, out$time),c(1, 11, 12, 9, 2:8)]))
}

#establish wich parameters can be estimated by the dataset
sF1 <- sensFun(func=Objective, parms=params[1:10], varscale=1) 
summary(sF1)
coll1 = collin(sF1)
plot(coll1, log="y")
abline(h=20, col="red") #if collinearity is less than 20, it is generally okay to estimate those parameters
head(coll1)
coll1[coll1$collinearity<20 & coll1$N ==7,]


##################MCMC using FME package###########


#First, I am going to use fake data that is the output from the model with the parameters that I specified above
#doing this will allow us to know if the MCMC is picking the correct values for the model



#Get fake data ready
head(out)
data.assim = out[,c(1:8, 11)]
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
plot(data.assim[,2])
plot(data.assim[,3])
plot(data.assim[,4])
plot(data.assim[,5])
plot(data.assim[,6])
plot(data.assim[,7])
plot(data.assim[,8])
plot(data.assim[,9])
plot(data.assim[,10])



#let's start with noninformative priors
#we don't need to specify a function for the priors because noninformative priors are the default for the functions used to fit the model and run the MCMC

#First, fit the model to the data

residuals <- function(params){ 
  modout = solvemodel(params)
  modout = data.frame(modout[,c(1:8, 11)]) #choose columns that you want
  modout1 = modout[match(data.assim$time, modout$time),] #only include time points for that have data
  resid = as.vector(unlist(data.assim[,2:length(data.assim)]-modout1[,2:length(modout1)]))
}

Fit = modFit(p=params, f=residuals, lower=rep(0, length=length(params)))
sFit = summary(Fit)

mse = sFit$var_ms_unweighted
Cov = sFit$cov.sclaed
print(system.time(modMCMC(f=residual, p=Fit$par, jum=Cov, lower=c(0,0), )))