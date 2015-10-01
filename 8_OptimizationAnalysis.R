#use this script to validate model and optimization
#also includes variance decomposition analysis


#Load workspace and save the summary statistics to summary table
load("Step2_NEE_NDVI_UNBdata_MELstarting.Rdata") #load workspace
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
summarytable=data.frame(q05 = q05, q25 = q25, mean = means, 
                        q75 = q75, q95 = q95) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_NEE_NDVI_UNBdata = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_NEE_NDVI_UNBdata, "Params_NEE_NDVI_UNBdata.csv")

###comparison using data that was assimilated
data.compare1 = data.frame(data.compare1)
out=data.frame(solvemodel(param.best, state)) #with columns to match data.assim
out.compare1 = out[match(data.compare1$time, out$time),]
out.compare1=out.compare1[,c(1,7,11)]
head(out.compare1)
head(data.compare1)

#now calculate bias mean error, MAE, and R2 for each stock/flux of interest

#calculate RMSE
error = (data.compare1[,c(2,3)]-out.compare1[,c(2,3)])
errorsquared = error^2
mean = apply(errorsquared,2,mean,na.rm=TRUE)
RMSE = sqrt(mean)
#calculate MAE
abs.error = abs(out.compare1-data.compare1)
MAE = apply(abs.error[,c(2,3)],2,mean,na.rm=TRUE)
#calculate r2
reg_NEE = lm(data.compare1[,2]~out.compare1[,2])
r2_NEE = summary(reg_NEE)$r.squared
reg_NDVI = lm(data.compare1[,3]~out.compare1[,3])
r2_NDVI = summary(reg_NDVI)$r.squared

##plot linear regression for assimilated data

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data.compare1$NEE, out.compare1$NEE, xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

plot(data.compare1$NDVI, out.compare1$NDVI, xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out.compare1$NEE~out.compare1$time, pch=16)
points(data.compare1$NEE~data.compare1$time, col="red")

plot(out.compare1$NDVI~out.compare1$time, pch=16)
points(data.compare1$NDVI~data.compare1$time, col="red")


###comparison using data that was NOT assimilated
data.compare=read.csv("Assimilation_data_ALL.csv")
data.compare1 = data.compare[data.compare$Year==2011,]

data.compare1 = data.frame(data.compare1)
out=data.frame(solvemodel(param.best, state)) #with columns to match data.assim
out.compare1 = out[match(data.compare1$Time, out$time),]
out.compare1=out.compare1[,c(1,7,8,9,11)]
data.compare1=data.compare1[,c(3,6,7,8,9)]
head(out.compare1)
head(data.compare1)

#now calculate bias mean error, MAE, and R2 for each stock/flux of interest

#calculate RMSE
error = (data.compare1[,c(2:5)]-out.compare1[,c(2:5)])
errorsquared = error^2
mean = apply(errorsquared,2,mean,na.rm=TRUE)
RMSE = sqrt(mean)
#calculate MAE
abs.error = abs(out.compare1-data.compare1)
MAE = apply(abs.error,2,mean,na.rm=TRUE)
#calculate r2
reg_NEE = lm(data.compare1[,2]~out.compare1[,2])
r2_NEE = summary(reg_NEE)$r.squared
reg_GPP = lm(data.compare1[,3]~out.compare1[,3])
r2_GPP = summary(reg_GPP)$r.squared
reg_Re = lm(data.compare1[,4]~out.compare1[,4])
r2_Re = summary(reg_Re)$r.squared
reg_NDVI = lm(data.compare1[,5]~out.compare1[,5])
r2_NDVI = summary(reg_NDVI)$r.squared

##plot linear regression for assimilated data

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data.compare1[,2], out.compare1[,2], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

plot(data.compare1[,4], out.compare1[,4], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out.compare1$NEE~out.compare1$time, pch=16)
points(data.compare1$NEE~data.compare1$Time, col="red")

plot(out.compare1$NDVI~out.compare1$time, pch=16)
points(data.compare1$NDVI~data.compare1$Time, col="red")










##########################VARIANCE DECOMPOSITION ANALYSIS#######################

load("Step2_NEE_NDVI_UNBdata_MELstarting.Rdata") #load best experiment (experiment 4)
head(param.keep) #view table of accepted parameters
means=apply(param.keep, 2, mean) #calculate parameter means

#to perform the variance decomposition analysis, you need to:
# 1) alter each parameter individually holding all other parameters constant at their means
# 2) run the model for each parameter set to obtain an ensemble of model runs
# 3) for each model run, calculate the monthly average of the output
# 4) Calculate the variance in monthly averages for each parameter - this gives you the contribution of that parameter to the model variance

head(out[,1:11])


####SEASONAL ANALYSIS####
#need to make tables to store monthly averages for each model output (each parameter has one table)

MVar_kplant = data.frame(matrix(1,1,11))
colnames(MVar_kplant)=c("Month", colnames(out[,2:11]))
MVar_LitterRateC = data.frame(matrix(1,1,11))
colnames(MVar_LitterRateC)=c("Month", colnames(out[,2:11]))
MVar_LitterRateN = data.frame(matrix(1,1,11))
colnames(MVar_LitterRateN)=c("Month", colnames(out[,2:11]))
MVar_RespRate = data.frame(matrix(1,1,11))
colnames(MVar_RespRate)=c("Month", colnames(out[,2:11]))
MVar_UptakeRate = data.frame(matrix(1,1,11))
colnames(MVar_UptakeRate)=c("Month", colnames(out[,2:11]))
MVar_propN_fol = data.frame(matrix(1,1,11))
colnames(MVar_propN_fol)=c("Month", colnames(out[,2:11]))
MVar_propN_roots = data.frame(matrix(1,1,11))
colnames(MVar_propN_roots)=c("Month", colnames(out[,2:11]))
MVar_q10 = data.frame(matrix(1,1,11))
colnames(MVar_q10)=c("Month", colnames(out[,2:11]))
MVar_netNrate = data.frame(matrix(1,1,11))
colnames(MVar_netNrate)=c("Month", colnames(out[,2:11]))
MVar_cue = data.frame(matrix(1,1,11))
colnames(MVar_cue)=c("Month", colnames(out[,2:11]))


#need to create a vector of months to append to model output
months = rep(c(seq(1:12)),
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
months.leap = rep(c(seq(1:12)),
                  c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

months = c(months, months, months, months.leap)

#kplant
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_kplant)
  MVar_kplant = rbind(MVar_kplant, monthly.avg)
}  

#LitterRateC
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_LitterRateC)
  MVar_LitterRateC = rbind(MVar_LitterRateC, monthly.avg)
}  

#LitterRateN
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_LitterRateN)
  MVar_LitterRateN = rbind(MVar_LitterRateN, monthly.avg)
}  

#RespRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_RespRate)
  MVar_RespRate = rbind(MVar_RespRate, monthly.avg)
}  

#UptakeRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_UptakeRate)
  MVar_UptakeRate = rbind(MVar_UptakeRate, monthly.avg)
}  

#propN_fol
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_fol)
  MVar_propN_fol = rbind(MVar_propN_fol, monthly.avg)
}  

#propN_roots
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_roots)
  MVar_propN_roots = rbind(MVar_propN_roots, monthly.avg)
}  

#q10
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_q10)
  MVar_q10 = rbind(MVar_q10, monthly.avg)
}  

#netNrate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_netNrate)
  MVar_netNrate = rbind(MVar_netNrate, monthly.avg)
}  

#cue
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[10] = unlist(c(param.keep[i,10]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_cue)
  MVar_cue = rbind(MVar_cue, monthly.avg)
}  


MVar_kplant = MVar_kplant[-1,]
MVar_LitterRateC = MVar_LitterRateC[-1,]
MVar_LitterRateN = MVar_LitterRateN[-1,]
MVar_RespRate = MVar_RespRate[-1,]
MVar_UptakeRate = MVar_UptakeRate[-1,]
MVar_propN_fol = MVar_propN_fol[-1,]
MVar_propN_roots = MVar_propN_roots[-1,]
MVar_q10 = MVar_q10[-1,]
MVar_netNrate = MVar_netNrate[-1,]
MVar_cue = MVar_cue[-1,]



var.kplant = aggregate(MVar_kplant[,2:11], list(MVar_kplant$Month), var)
var.LitterRateC = aggregate(MVar_LitterRateC[,2:11], list(MVar_LitterRateC$Month), var)
var.LitterRateN = aggregate(MVar_LitterRateN[,2:11], list(MVar_LitterRateN$Month), var)
var.RespRate = aggregate(MVar_RespRate[,2:11], list(MVar_RespRate$Month), var)
var.UptakeRate = aggregate(MVar_UptakeRate[,2:11], list(MVar_UptakeRate$Month), var)
var.propN_fol = aggregate(MVar_propN_fol[,2:11], list(MVar_propN_fol$Month), var)
var.propN_roots = aggregate(MVar_propN_roots[,2:11], list(MVar_propN_roots$Month), var)
var.q10 = aggregate(MVar_q10[,2:11], list(MVar_q10$Month), var)
var.netNrate = aggregate(MVar_netNrate[,2:11], list(MVar_netNrate$Month), var)
var.cue = aggregate(MVar_cue[,2:11], list(MVar_cue$Month), var)

parameters = rep(names(params), c(12,12,12,12,12,12,12,12,12,12))

all = rbind(var.kplant, var.LitterRateC, var.LitterRateN, var.RespRate, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.q10, var.netNrate, var.cue)

all=cbind(Parameters = parameters, all)

#calculate total variance
var.total = aggregate(all[3:12], list(all$Group.1), sum)  #CHECK THIS

#now calculate percent variance
perc.kplant = (var.kplant[,2:11]/var.total[,2:11])*100
perc.kplant = cbind(Parameter = rep("kplant", 12), Month=var.total$Group.1, perc.kplant)

perc.LitterRateC = (var.LitterRateC[,2:11]/var.total[,2:11])*100
perc.LitterRateC = cbind(Parameter = rep("LitterRateC", 12), Month=var.total$Group.1, perc.LitterRateC)

perc.LitterRateN = (var.LitterRateN[,2:11]/var.total[,2:11])*100
perc.LitterRateN = cbind(Parameter = rep("LitterRateN", 12), Month=var.total$Group.1, perc.LitterRateN)

perc.RespRate = (var.RespRate[,2:11]/var.total[,2:11])*100
perc.RespRate = cbind(Parameter = rep("RespRate", 12), Month=var.total$Group.1, perc.RespRate)

perc.UptakeRate = (var.UptakeRate[,2:11]/var.total[,2:11])*100
perc.UptakeRate = cbind(Parameter = rep("UptakeRate", 12), Month=var.total$Group.1, perc.UptakeRate)

perc.propN_fol = (var.propN_fol[,2:11]/var.total[,2:11])*100
perc.propN_fol = cbind(Parameter = rep("propN_fol", 12), Month=var.total$Group.1, perc.propN_fol)

perc.propN_roots = (var.propN_roots[,2:11]/var.total[,2:11])*100
perc.propN_roots = cbind(Parameter = rep("propN_roots", 12), Month=var.total$Group.1, perc.propN_roots)

perc.q10 = (var.q10[,2:11]/var.total[,2:11])*100
perc.q10 = cbind(Parameter = rep("q10", 12), Month=var.total$Group.1, perc.q10)

perc.netNrate = (var.netNrate[,2:11]/var.total[,2:11])*100
perc.netNrate = cbind(Parameter = rep("netNrate", 12), Month=var.total$Group.1, perc.netNrate)

perc.cue = (var.cue[,2:11]/var.total[,2:11])*100
perc.cue = cbind(Parameter = rep("cue", 12), Month=var.total$Group.1, perc.cue)


#create a table binding all together

perc.all = rbind(perc.kplant, perc.LitterRateC, perc.LitterRateN, perc.RespRate,
                 perc.UptakeRate, perc.propN_fol, perc.propN_roots, perc.q10, perc.netNrate,
                 perc.cue)

perc.all = perc.all[,-11]
head(perc.all)
tail(perc.all)

####barplots####

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 3:11) { #for each parameter
  sub = perc.all[,c(1,2,n)]
  sub1 = table(sub$Parameter, sub$Month)
  sub1[1,] = sub[1:12,3]
  sub1[2,] = sub[13:24,3]
  sub1[3,] = sub[25:36,3]
  sub1[4,] = sub[37:48,3]
  sub1[5,] = sub[49:60,3]
  sub1[6,] = sub[61:72,3]
  sub1[7,] = sub[73:84,3]
  sub1[8,] = sub[85:96,3]
  sub1[9,] = sub[97:108,3]
  sub1[10,] = sub[109:120,3]
  sub1[11,] = sub[121:132,3]
  barplot(sub1, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "darkseagreen", 
                      "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1", "purple"),            
          main=names(perc.all[n]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100)) #plot the data
} #end of for loop


par(mfrow=c(1,1), mar=c(4,4,2,2))


#NEE
sub = perc.all[,c(1,2,8)]
sub1 = table(sub$Parameter, sub$Month)
sub1[1,] = sub[1:12,3]
sub1[2,] = sub[13:24,3]
sub1[3,] = sub[25:36,3]
sub1[4,] = sub[37:48,3]
sub1[5,] = sub[49:60,3]
sub1[6,] = sub[61:72,3]
sub1[7,] = sub[73:84,3]
sub1[8,] = sub[85:96,3]
sub1[9,] = sub[97:108,3]
sub1[10,] = sub[109:120,3]
sub1[11,] = sub[121:132,3]
barplot(sub1, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "darkseagreen", 
                    "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1", "purple"),            
        main=names(perc.all[8]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100), legend=TRUE) #plot the data


####ANNUAL ANALYSIS####
#need to make tables to store annual sums for each model output (each parameter has one table)

AVar_kplant = data.frame(matrix(1,1,3))
colnames(AVar_kplant)=colnames(out[,7:9])
AVar_LitterRateC = data.frame(matrix(1,1,3))
colnames(AVar_LitterRateC)=colnames(out[,7:9])
AVar_LitterRateN = data.frame(matrix(1,1,3))
colnames(AVar_LitterRateN)=colnames(out[,7:9])
AVar_RespRate = data.frame(matrix(1,1,3))
colnames(AVar_RespRate)=colnames(out[,7:9])
AVar_UptakeRate = data.frame(matrix(1,1,3))
colnames(AVar_UptakeRate)=colnames(out[,7:9])
AVar_propN_fol = data.frame(matrix(1,1,3))
colnames(AVar_propN_fol)=colnames(out[,7:9])
AVar_propN_roots = data.frame(matrix(1,1,3))
colnames(AVar_propN_roots)=colnames(out[,7:9])
AVar_q10 = data.frame(matrix(1,1,3))
colnames(AVar_q10)=colnames(out[,7:9])
AVar_netNrate = data.frame(matrix(1,1,3))
colnames(AVar_netNrate)=colnames(out[,7:9])
AVar_cue = data.frame(matrix(1,1,3))
colnames(AVar_cue)=colnames(out[,7:9])


#kplant
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_kplant) #change names
  AVar_kplant = rbind(AVar_kplant, annual.avg) #add row to table
}  

#LitterRateC
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_LitterRateC) #change names
  AVar_LitterRateC = rbind(AVar_LitterRateC, annual.avg) #add row to table
}  

#LitterRateN
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_LitterRateN) #change names
  AVar_LitterRateN = rbind(AVar_LitterRateN, annual.avg) #add row to table
}  

#RespRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_RespRate) #change names
  AVar_RespRate = rbind(AVar_RespRate, annual.avg) #add row to table
}  

#UptakeRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_UptakeRate) #change names
  AVar_UptakeRate = rbind(AVar_UptakeRate, annual.avg) #add row to table
}  

#propN_fol
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_propN_fol) #change names
  AVar_propN_fol = rbind(AVar_propN_fol, annual.avg) #add row to table
}  

#propN_roots
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_propN_roots) #change names
  AVar_propN_roots = rbind(AVar_propN_roots, annual.avg) #add row to table
}  

#q10
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_q10) #change names
  AVar_q10 = rbind(AVar_q10, annual.avg) #add row to table
}  

#netNrate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_netNrate) #change names
  AVar_netNrate = rbind(AVar_netNrate, annual.avg) #add row to table
}  

#cue
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[10] = unlist(c(param.keep[i,10]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_cue) #change names
  AVar_cue = rbind(AVar_cue, annual.avg) #add row to table
}  





AVar_kplant = AVar_kplant[-1,]
AVar_LitterRateC = AVar_LitterRateC[-1,]
AVar_LitterRateN = AVar_LitterRateN[-1,]
AVar_RespRate = AVar_RespRate[-1,]
AVar_UptakeRate = AVar_UptakeRate[-1,]
AVar_propN_fol = AVar_propN_fol[-1,]
AVar_propN_roots = AVar_propN_roots[-1,]
AVar_q10 = AVar_q10[-1,]
AVar_netNrate = AVar_netNrate[-1,]
AVar_cue = AVar_cue[-1,]



var.kplant = apply(AVar_kplant, 2, var)
var.LitterRateC = apply(AVar_LitterRateC, 2, var)
var.LitterRateN = apply(AVar_LitterRateN, 2, var)
var.RespRate = apply(AVar_RespRate, 2, var)
var.UptakeRate = apply(AVar_UptakeRate, 2, var)
var.propN_fol = apply(AVar_propN_fol, 2, var)
var.propN_roots = apply(AVar_propN_roots, 2, var)
var.q10 = apply(AVar_q10, 2, var)
var.netNrate = apply(AVar_netNrate, 2, var)
var.cue = apply(AVar_cue, 2, var)

parameters = names(params)

all_1 = rbind(var.kplant, var.LitterRateC, var.LitterRateN, var.RespRate, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.q10, var.netNrate, var.cue)

all_1 = cbind(Parameters = parameters, all)

#calculate total variance
var.total = apply(all_1[2:4], sum)

#now calculate percent variance
perc.kplant = (var.kplant/var.total)*100
perc.LitterRateC = (var.LitterRateC/var.total)*100
perc.LitterRateN = (var.LitterRateN/var.total)*100
perc.RespRate = (var.RespRate/var.total)*100
perc.UptakeRate = (var.UptakeRate/var.total)*100
perc.propN_fol = (var.propN_fol/var.total)*100
perc.propN_roots = (var.propN_roots/var.total)*100
perc.q10 = (var.q10/var.total)*100
perc.netNrate = (var.netNrate/var.total)*100
perc.cue = (var.cue/var.total)*100

#create a table binding all together

perc.all_1 = rbind(perc.kplant, perc.LitterRateC, perc.LitterRateN, perc.RespRate,
                 perc.UptakeRate, perc.propN_fol, perc.propN_roots, perc.q10, perc.netNrate,
                 perc.cue)

perc.all_1 = cbind(Parameters=parameters, perc.all_1
head(perc.all_1)
tail(perc.all_1)

####barplots####

#START EDITING HERE

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 3:11) { #for each parameter
  sub = perc.all[,c(1,2,n)]
  sub1 = table(sub$Parameter, sub$Month)
  sub1[1,] = sub[1:12,3]
  sub1[2,] = sub[13:24,3]
  sub1[3,] = sub[25:36,3]
  sub1[4,] = sub[37:48,3]
  sub1[5,] = sub[49:60,3]
  sub1[6,] = sub[61:72,3]
  sub1[7,] = sub[73:84,3]
  sub1[8,] = sub[85:96,3]
  sub1[9,] = sub[97:108,3]
  sub1[10,] = sub[109:120,3]
  sub1[11,] = sub[121:132,3]
  barplot(sub1, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "darkseagreen", 
                      "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1", "purple"),            
          main=names(perc.all[n]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100)) #plot the data
} #end of for loop


par(mfrow=c(1,1), mar=c(4,4,2,2))


#NEE
sub = perc.all[,c(1,2,8)]
sub1 = table(sub$Parameter, sub$Month)
sub1[1,] = sub[1:12,3]
sub1[2,] = sub[13:24,3]
sub1[3,] = sub[25:36,3]
sub1[4,] = sub[37:48,3]
sub1[5,] = sub[49:60,3]
sub1[6,] = sub[61:72,3]
sub1[7,] = sub[73:84,3]
sub1[8,] = sub[85:96,3]
sub1[9,] = sub[97:108,3]
sub1[10,] = sub[109:120,3]
sub1[11,] = sub[121:132,3]
barplot(sub1, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "darkseagreen", 
                    "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1", "purple"),            
        main=names(perc.all[8]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100), legend=TRUE) #plot the data


save.image(file="Variance_09252015.Rdata")

