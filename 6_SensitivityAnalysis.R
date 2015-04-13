#############PLOTS OF DISTRIBUTIONS###############

par(mfrow=c(5,2), mar=c(4,4,2,2))
#kplant
plot(density(param.keep$kplant), main="kplant", xlab="", ylab="") #plot distribution
abline(v=param.best[1], col="green", lwd=3)
abline(v=params[1], col="red", lwd=3) #add line where actual value should be

#LitterRate
plot(density(param.keep$LitterRate), main="LitterRate", xlab="", ylab="") #plot distribution
abline(v=param.best[2], col="green", lwd=3)
abline(v=params[2], col="red", lwd=3) #add line where actual value should be

#DecompRateC
plot(density(param.keep$DecompRateC), main="DecompRateC", xlab="", ylab="") #plot distribution
abline(v=param.best[3], col="green", lwd=3)
abline(v=as.numeric(params$DecompRateC), col="red", lwd=3) #add line where actual value should be

#DecompRateN
plot(density(param.keep$DecompRateN), main="DecompRateN", xlab="", ylab="") #plot distribution
abline(v=param.best[4], col="green", lwd=3)
abline(v=as.numeric(params$DecompRateN), col="red", lwd=3) #add line where actual value should be

#retrans
plot(density(param.keep$retrans), main="retrans", xlab="", ylab="") #plot distribution
abline(v=param.best[5], col="green", lwd=3)
abline(v=as.numeric(params$retrans), col="red", lwd=3) #add line where actual value should be

#RespRateSOM
plot(density(param.keep$RespRateSOM), main="RespRateSOM", xlab="", ylab="") #plot distribution
abline(v=param.best[6], col="green", lwd=3)
abline(v=as.numeric(params$RespRateSOM), col="red", lwd=3) #add line where actual value should be

#PropResp
plot(density(param.keep$PropResp), main="PropResp", xlab="", ylab="") #plot distribution
abline(v=param.best[7], col="green", lwd=3)
abline(v=as.numeric(params$PropResp), col="red", lwd=3) #add line where actual value should be

#UptakeRate
plot(density(param.keep$UptakeRate), main="UptakeRate", xlab="", ylab="") #plot distribution
abline(v=param.best[8], col="green", lwd=3)
abline(v=as.numeric(params$UptakeRate), col="red", lwd=3) #add line where actual value should be

#netNrate
plot(density(param.keep$netNrate), main="netNrate", xlab="", ylab="") #plot distribution
abline(v=param.best[9], col="green", lwd=3)
abline(v=as.numeric(params$netNrate), col="red", lwd=3) #add line where actual value should be

#q10
plot(density(param.keep$q10), main="Biomass_C", xlab="", ylab="") #plot distribution
abline(v=param.best[10], col="green", lwd=3)
abline(v=as.numeric(params$q10) col="red", lwd=3) #add line where actual value should be



##########Plot effects of estimated parameters on model output###################

sensvars = c("Biomass_C", 
             "Biomass_N", 
             "Litter_C", 
             "Litter_N", 
             "SOM_C", 
             "SOM_N",
             "Available_N",
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


#calcualte variance covariance matrix

install.packages("MBESS")
require(MBESS)

sdevs = apply(param.keep, 2, sd) #calculate the standard deviations of each variable
params.cov = cor2cov(param.cor, sdevs) #calculates covariance matrix from correlation matrix

#global sensitivity analysis

parms = as.vector(unlist(params)) #create vector of parameters
params.mean =  apply(param.keep, 2, mean)
params.min =  apply(param.keep, 2, min)
params.max =  apply(param.keep, 2, max)
params.cov #calculated above
parRanges = data.frame(min = params.min,  max = params.max)
rownames(parRanges) = names(params)
parRanges

s.global <- sensRange(func=solvemodel, parms=params, sensvar = sensvars, dist ="norm", 
                      parRange=parRanges, parMean = params.mean, parCovar=params.cov, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "Time (days)", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


