###This is similar to the synthetic data experiment analysis, but doesn't include comparison of experiments###


#Load workspace and save the summary statistics to summary table
load("Step2_NEE_UNBdata_05.Rdata") #load workspace
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
parameters = names(param.best) #create vector of parameter names
summarytable=data.frame(Parameter = parameters, q05 = q05, q25 = q25, mean = means, 
                        q75 = q75, q95 = q95, diff = diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_NEE_UNBdata_05 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_NEE_UNBdata_05, "Params_NEE_UNBdata_05.csv")


####statistical calculations####

out=data.frame(solvemodel(param.best, state))[,7] #with columns to match data.assim
out.compare1 = out[match(data.compare1$time, out$time),7]
head(out.compare1)
head(data.compare1)

#now calculate bias mean error, MAE, and R2 for each stock/flux of interest

#calculate RMSE
error = (data.compare1-out.compare1)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE = sqrt(mean)
#calculate MAE
abs.error = abs(out.compare1-data.compare1)
MAE = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg = lm(data.compare1[,1]~out.compare1[,1])
r2 = summary(reg)$r.squared

##plot linear regression for assimilated data

par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(data.compare1, out.compare1, xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
plot(density(resid(reg)), main="Density of Residuals")

#Now, load 2011 data and compare to predicted