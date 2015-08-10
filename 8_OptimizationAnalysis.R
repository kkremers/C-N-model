###This is similar to the synthetic data experiment analysis, but doesn't include comparison of experiments###


#Load workspace and save the summary statistics to summary table
load("Step2_NEE_UNBdata.Rdata") #load workspace
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
summarytable=data.frame(q05 = q05, q25 = q25, mean = means, 
                        q75 = q75, q95 = q95, diff = diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_NEE_UNBdata_05 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_NEE_UNBdata, "Params_NEE_UNBdata_05.csv")


####statistical calculations####
data.compare1 = data.frame(data.compare1)
out=data.frame(solvemodel(param.best, state)) #with columns to match data.assim
out.compare1 = out[match(data.compare1$time, out$time),]
out.compare1=out.compare1[,c(1,7)]
head(out.compare1)
head(data.compare1)

#now calculate bias mean error, MAE, and R2 for each stock/flux of interest

#calculate RMSE
error = (data.compare1[,2]-out.compare1[,2])
errorsquared = error^2
mean = mean(errorsquared[!is.na(errorsquared)])
RMSE = sqrt(mean)
#calculate MAE
abs.error = abs(out.compare1-data.compare1)
MAE = mean(abs.error[!is.na(abs.error)])
#calculate r2
reg = lm(data.compare1[,2]~out.compare1[,2])
r2 = summary(reg)$r.squared

##plot linear regression for assimilated data

par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(data.compare1[,2], out.compare1[,2], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
plot(density(resid(reg)), main="Density of Residuals")

par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(out.compare1$NEE~out.compare1$time, pch=16)
points(data.compare1$NEE~data.compare1$time, pch=16, col="red")


#Now, load 2011 data and compare to predicted