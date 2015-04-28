#############PLOTS OF DISTRIBUTIONS###############

par(mfrow=c(4,3), mar=c(4,4,2,2))
hist(param.keep[,1], main=names(params[1]))
abline(v=params[1], col="red", lwd=3)
abline(v=param.best[1], col="green", lwd=3)
hist(param.keep[,2], main=names(params[2]))
abline(v=params[2], col="red", lwd=3)
abline(v=param.best[2], col="green", lwd=3)
hist(param.keep[,3], main=names(params[3]))
abline(v=params[3], col="red", lwd=3)
abline(v=param.best[3], col="green", lwd=3)
hist(param.keep[,4], main=names(params[4]))
abline(v=params[4], col="red", lwd=3)
abline(v=param.best[4], col="green", lwd=3)
hist(param.keep[,5], main=names(params[5]))
abline(v=params[5], col="red", lwd=3)
abline(v=param.best[5], col="green", lwd=3)
hist(param.keep[,6], main=names(params[6]))
abline(v=params[6], col="red", lwd=3)
abline(v=param.best[6], col="green", lwd=3)
hist(param.keep[,7], main=names(params[7]))
abline(v=params[7], col="red", lwd=3)
abline(v=param.best[7], col="green", lwd=3)
hist(param.keep[,8], main=names(params[8]))
abline(v=params[8], col="red", lwd=3)
abline(v=param.best[8], col="green", lwd=3)
hist(param.keep[,9], main=names(params[9]))
abline(v=params[9], col="red", lwd=3)
abline(v=param.best[9], col="green", lwd=3)


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
s.local <- sensFun(func=solvemodel, parms=param.best, state=state, sensvar = sensvars, varscale=1)

head(s.local); tail(s.local)
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
params.cov = cor2cov(as.matrix(param.cor), sdevs) #calculates covariance matrix from correlation matrix

#global sensitivity analysis

params.mean =  apply(param.keep, 2, mean)
params.min =  apply(param.keep, 2, min)
params.max =  apply(param.keep, 2, max)
params.cov #calculated above
parRanges = data.frame(min = params.min,  max = params.max)
rownames(parRanges) = names(params)
parRanges

s.global <- sensRange(func=solvemodel, parms=param.best, , state=state, sensvar = sensvars, dist ="norm", 
                      parRange=parRanges, parMean = params.mean, parCovar=params.cov, num=50)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(3,2)) 
plot(s.global.summ, xlab = "Time (days)", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


