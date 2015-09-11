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
                        q75 = q75, q95 = q95, diff = diff) #bind all of the information together in the proper order (same order as summarytable columns)
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
mean = mean(errorsquared[!is.na(errorsquared)])
RMSE = sqrt(mean)
#calculate MAE
abs.error = abs(out.compare1-data.compare1)
MAE_NEE = mean(abs.error[!is.na(abs.error)])
#calculate r2
reg_NEE = lm(data.compare1[,2]~out.compare1[,2])
r2_NEE = summary(reg_NEE)$r.squared
reg_NDVI = lm(data.compare1[,3]~out.compare1[,3])
r2_NDVI = summary(reg_NDVI)$r.squared

##plot linear regression for assimilated data

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data.compare1[,2], out.compare1[,2], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

plot(data.compare1[,3], out.compare1[,3], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out.compare1$NEE~out.compare1$time, pch=16)
points(data.compare1$NEE~data.compare1$time, col="red")

plot(out.compare1$NDVI~out.compare1$time, pch=16)
points(data.compare1$NDVI~data.compare1$time, col="red")


###comparison using data that was NOT assimilated
data.compare=read.csv("Assimilation_data_ALL.csv")
data.compare1 = data.compare[Year==2011,]

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
mean = mean(errorsquared[!is.na(errorsquared)])
RMSE = sqrt(mean)
#calculate MAE
abs.error = abs(out.compare1-data.compare1)
MAE_NEE = mean(abs.error[!is.na(abs.error)])
#calculate r2
reg_NEE = lm(data.compare1[,2]~out.compare1[,2])
r2_NEE = summary(reg_NEE)$r.squared
reg_NDVI = lm(data.compare1[,3]~out.compare1[,3])
r2_NDVI = summary(reg_NDVI)$r.squared

##plot linear regression for assimilated data

par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data.compare1[,2], out.compare1[,2], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

plot(data.compare1[,3], out.compare1[,3], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out.compare1$NEE~out.compare1$time, pch=16)
points(data.compare1$NEE~data.compare1$time, col="red")

plot(out.compare1$NDVI~out.compare1$time, pch=16)
points(data.compare1$NDVI~data.compare1$time, col="red")










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
MVar_LitterRate = data.frame(matrix(1,1,11))
colnames(MVar_LitterRate)=c("Month", colnames(out[,2:11]))
MVar_retrans = data.frame(matrix(1,1,11))
colnames(MVar_retrans)=c("Month", colnames(out[,2:11]))
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
MVar_temp2_resp = data.frame(matrix(1,1,11))
colnames(MVar_temp2_resp)=c("Month", colnames(out[,2:11]))
MVar_temp2_netn = data.frame(matrix(1,1,11))
colnames(MVar_temp2_netn)=c("Month", colnames(out[,2:11]))


#need to create a vector of months to append to model output
months = rep(c(seq(1:12)),
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
months.leap = rep(c(seq(1:12)),
                  c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

months = c(months, months, months, months.leap, months)

#kplant
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_kplant)
  MVar_kplant = rbind(MVar_kplant, monthly.avg)
}  

#LitterRate
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_LitterRate)
  MVar_LitterRate = rbind(MVar_LitterRate, monthly.avg)
}  

#retrans
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_retrans)
  MVar_retrans = rbind(MVar_retrans, monthly.avg)
}  

#RespRate
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_RespRate)
  MVar_RespRate = rbind(MVar_RespRate, monthly.avg)
}  

#UptakeRate
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_UptakeRate)
  MVar_UptakeRate = rbind(MVar_UptakeRate, monthly.avg)
}  

#propN_fol
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_fol)
  MVar_propN_fol = rbind(MVar_propN_fol, monthly.avg)
}  

#propN_roots
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_roots)
  MVar_propN_roots = rbind(MVar_propN_roots, monthly.avg)
}  

#q10
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_q10)
  MVar_q10 = rbind(MVar_q10, monthly.avg)
}  

#netNrate
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_netNrate)
  MVar_netNrate = rbind(MVar_netNrate, monthly.avg)
}  

#temp2_resp
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[10] = unlist(c(param.keep[i,10]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_temp2_resp)
  MVar_temp2_resp = rbind(MVar_temp2_resp, monthly.avg)
}  

#temp2_netn
for(i in 1:100){
  params.i = means #set parmeters to mean values
  params.i[11] = unlist(c(param.keep[i,11]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_temp2_netn)
  MVar_temp2_netn = rbind(MVar_temp2_netn, monthly.avg)
}  

MVar_kplant = MVar_kplant[-1,]
MVar_LitterRate = MVar_LitterRate[-1,]
MVar_retrans = MVar_retrans[-1,]
MVar_RespRate = MVar_RespRate[-1,]
MVar_UptakeRate = MVar_UptakeRate[-1,]
MVar_propN_fol = MVar_propN_fol[-1,]
MVar_propN_roots = MVar_propN_roots[-1,]
MVar_q10 = MVar_q10[-1,]
MVar_netNrate = MVar_netNrate[-1,]
MVar_temp2_resp = MVar_temp2_resp[-1,]
MVar_temp2_netn = MVar_temp2_netn[-1,]


var.kplant = aggregate(MVar_kplant[,2:11], list(MVar_kplant$Month), var)
var.LitterRate = aggregate(MVar_LitterRate[,2:11], list(MVar_LitterRate$Month), var)
var.retrans = aggregate(MVar_retrans[,2:11], list(MVar_retrans$Month), var)
var.RespRate = aggregate(MVar_RespRate[,2:11], list(MVar_RespRate$Month), var)
var.UptakeRate = aggregate(MVar_UptakeRate[,2:11], list(MVar_UptakeRate$Month), var)
var.propN_fol = aggregate(MVar_propN_fol[,2:11], list(MVar_propN_fol$Month), var)
var.propN_roots = aggregate(MVar_propN_roots[,2:11], list(MVar_propN_roots$Month), var)
var.q10 = aggregate(MVar_q10[,2:11], list(MVar_q10$Month), var)
var.netNrate = aggregate(MVar_netNrate[,2:11], list(MVar_netNrate$Month), var)
var.temp2_resp = aggregate(MVar_temp2_resp[,2:11], list(MVar_temp2_resp$Month), var)
var.temp2_netn = aggregate(MVar_temp2_netn[,2:11], list(MVar_temp2_netn$Month), var)

parameters = rep(names(params), c(12,12,12,12,12,12,12,12,12,12,12))

all = rbind(var.kplant, var.LitterRate, var.retrans, var.RespRate, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.q10, var.netNrate, var.temp2_resp, var.temp2_netn)

all=cbind(Parameters = parameters, all)

#calculate total variance
var.total = aggregate(all[3:12], list(all$Group.1), sum)

#now calculate percent variance
perc.kplant = (var.kplant[,2:11]/var.total[,2:11])*100
perc.kplant = cbind(Parameter = rep("kplant", 12), Month=var.total$Group.1, perc.kplant)

perc.LitterRate = (var.LitterRate[,2:11]/var.total[,2:11])*100
perc.LitterRate = cbind(Parameter = rep("LitterRate", 12), Month=var.total$Group.1, perc.LitterRate)

perc.retrans = (var.retrans[,2:11]/var.total[,2:11])*100
perc.retrans = cbind(Parameter = rep("retrans", 12), Month=var.total$Group.1, perc.retrans)

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

perc.temp2_resp = (var.temp2_resp[,2:11]/var.total[,2:11])*100
perc.temp2_resp = cbind(Parameter = rep("temp2_resp", 12), Month=var.total$Group.1, perc.temp2_resp)

perc.temp2_netn = (var.temp2_netn[,2:11]/var.total[,2:11])*100
perc.temp2_netn = cbind(Parameter = rep("temp2_netn", 12), Month=var.total$Group.1, perc.temp2_netn)

#create a table binding all together

perc.all = rbind(perc.kplant, perc.LitterRate, perc.retrans, perc.RespRate,
                 perc.UptakeRate, perc.propN_fol, perc.propN_roots, perc.q10, perc.netNrate,
                 perc.temp2_resp, perc.temp2_netn)

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
                      "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1", "purple", "blue"),            
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
                    "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1", "purple", "blue"),            
        main=names(perc.all[8]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100), legend=TRUE) #plot the data

#GPP
sub = perc.all[,c(1,2,9)]
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
barplot(sub1, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "darkseagreen", 
                    "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1"),            
        main=names(perc.all[9]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100)) #plot the data

#Re
sub = perc.all[,c(1,2,10)]
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
barplot(sub1, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "darkseagreen", 
                    "deepskyblue", "dodgerblue3", "forestgreen", "darkslategray1"),            
        main=names(perc.all[10]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100)) #plot the data

save.image(file="Variance_07142015.Rdata")
