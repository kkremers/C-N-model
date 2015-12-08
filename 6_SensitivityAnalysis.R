#############PLOTS OF DISTRIBUTIONS###############

par(mfrow=c(3,5), mar=c(4,4,2,2))
plot(density(param.keep[,1]), main=names(params[1]), cex.axis=2)
abline(v=param.best[1], col="red", lwd=3)
plot(density(param.keep[,2]), main=names(params[2]))
abline(v=param.best[2], col="red", lwd=3)
plot(density(param.keep[,3]), main=names(params[3]))
abline(v=param.best[3], col="red", lwd=3)
plot(density(param.keep[,4]), main=names(params[4]))
abline(v=param.best[4], col="red", lwd=3)
plot(density(param.keep[,5]), main=names(params[5]), cex.axis=2)
abline(v=param.best[5], col="red", lwd=3)
plot(density(param.keep[,6]), main=names(params[6]))
abline(v=param.best[6], col="red", lwd=3)
plot(density(param.keep[,7]), main=names(params[7]))
abline(v=param.best[7], col="red", lwd=3)
plot(density(param.keep[,8]), main=names(params[8]))
abline(v=param.best[8], col="red", lwd=3)
plot(density(param.keep[,9]), main=names(params[9]))
abline(v=param.best[9], col="red", lwd=3)
plot(density(param.keep[,10]), main=names(params[10]))
abline(v=param.best[10], col="red", lwd=3)
plot(density(param.keep[,11]), main=names(params[11]))
abline(v=param.best[11], col="red", lwd=3)
plot(density(param.keep[,12]), main=names(params[12]))
abline(v=param.best[12], col="red", lwd=3)
plot(density(param.keep[,13]), main=names(params[13]))
abline(v=param.best[13], col="red", lwd=3)
plot(density(param.keep[,14]), main=names(params[14]))
abline(v=param.best[14], col="red", lwd=3)

##########Plot effects of estimated parameters on model output###################

sensvars = c("Biomass_C", 
             "Biomass_N", 
             "SOM_C", 
             "SOM_N",
             "Available_N",
             "NEE",
             "NDVI")

#local sensitivity analysis
s.local <- sensFun(func=solvemodel, parms=param.best, sensvar = sensvars)

head(s.local); tail(s.local)
s.local.summ = data.frame(summary(s.local, var=T))
head(s.local.summ); tail(s.local.summ)
s.loc.summ.ordered = data.frame(s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] )
write.csv(s.loc.summ.ordered, "LocalSensitivityAnalysis_NEENDVI.csv") #univariate sensitivity
#make a bar graph 


#NEE
sub = subset(s.local.summ, var=="NEE")
barplot(abs(sub$Mean), names.arg=names(params), cex.names=0.5, 
        col="forestgreen", horiz=TRUE, main="NEE") #plot the data


#NDVI
sub = subset(s.local.summ, var=="NDVI")
barplot(abs(sub$Mean), names.arg=names(params), cex.names=0.5, 
        col="forestgreen", horiz=TRUE, main="NDVI") #plot the data
abline(v=0)


param.cor = data.frame(cor(s.local[,c(-1,-2)]))#table of parameter correlations
param.cor
write.csv(param.cor, "ParamCorr_NEENDVI.csv") #bivariate sensitivity
pairs(s.local)

#global sensitivity analysis
summarytable
range = data.frame(min=summarytable$q05, max=summarytable$q95)
rownames(range)=rownames(summarytable)
s.global_90 <- sensRange(func=solvemodel, parms=param.best, sensvar = sensvars, 
                         parRange=range, num=100)

s.global.summ = summary(s.global)
head(s.global.summ)
#plots 
par(mfrow=c(4,2)) 
plot(s.global.summ, xlab = "Time (days)", mfrow = NULL,
     quant = TRUE, col = c("lightblue", "darkblue"), legpos = "topright")


