#use this script to validate model and optimization
#also includes variance decomposition analysis


#Load workspace and save the summary statistics to summary table
load("Step2_NEE_NDVI_UNBdata.Rdata") #load workspace
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q15=apply(param.keep, 2, quantile, 0.15) #calculate 15% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
q40=apply(param.keep, 2, quantile, 0.40) #calculate 40% quantile
means=apply(param.keep, 2, mean)
q60=apply(param.keep, 2, quantile, 0.60) #calculate 60% quantile
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q85=apply(param.keep, 2, quantile, 0.85) #calculate 85% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
summarytable=data.frame(q05 = q05, q15=q15, q25 = q25, q40=q40, mean = means, 
                        q60=q60, q75 = q75, q85=q85, q95 = q95) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_NEE_NDVI_UNBdata = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_NEE_NDVI_UNBdata, "Params_NEE_NDVI_UNBdata.csv")

###comparison using data that was assimilated
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,9]))
out=data.frame(solvemodel(param.best)) #with columns to match data.assim
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

par(mfrow=c(2,1), mar=c(3,2,2,2))
plot(out.compare1$NEE~out.compare1$time, pch=16, ylim=c(-5,2), col="gray57")
points(data.compare1$NEE~data.compare1$time, col="blue")
abline(h=0)

plot(out.compare1$NDVI~out.compare1$time, pch=16, ylim=c(0,0.8), col="gray57")
points(data.compare1$NDVI~data.compare1$time, col="blue")


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
plot(out.compare1$NEE~out.compare1$time, pch=16, ylim=c(-3,1))
points(data.compare1$NEE~data.compare1$time, col="red")

plot(out.compare1$NDVI~out.compare1$time, pch=16, ylim=c(0, 0.8))
points(data.compare1$NDVI~data.compare1$time, col="red")






##########################VARIANCE DECOMPOSITION ANALYSIS#######################

load("Step2_NEE_NDVI.Rdata") #load best experiment (experiment 4)
head(param.keep) #view table of accepted parameters
summarytable
means


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
MVar_UptakeRate = data.frame(matrix(1,1,11))
colnames(MVar_UptakeRate)=c("Month", colnames(out[,2:11]))
MVar_propN_fol = data.frame(matrix(1,1,11))
colnames(MVar_propN_fol)=c("Month", colnames(out[,2:11]))
MVar_propN_roots = data.frame(matrix(1,1,11))
colnames(MVar_propN_roots)=c("Month", colnames(out[,2:11]))
MVar_netNrate = data.frame(matrix(1,1,11))
colnames(MVar_netNrate)=c("Month", colnames(out[,2:11]))
MVar_cue = data.frame(matrix(1,1,11))
colnames(MVar_cue)=c("Month", colnames(out[,2:11]))
MVar_beta = data.frame(matrix(1,1,11))
colnames(MVar_beta)=c("Month", colnames(out[,2:11]))
MVar_BiomassC = data.frame(matrix(1,1,11))
colnames(MVar_BiomassC)=c("Month", colnames(out[,2:11]))
MVar_BiomassN = data.frame(matrix(1,1,11))
colnames(MVar_BiomassN)=c("Month", colnames(out[,2:11]))
MVar_SOMC = data.frame(matrix(1,1,11))
colnames(MVar_SOMC)=c("Month", colnames(out[,2:11]))
MVar_SOMN = data.frame(matrix(1,1,11))
colnames(MVar_SOMN)=c("Month", colnames(out[,2:11]))
MVar_AvailN = data.frame(matrix(1,1,11))
colnames(MVar_AvailN)=c("Month", colnames(out[,2:11]))

#need to create a vector of months to append to model output
months = rep(c(seq(1:12)),
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
months.leap = rep(c(seq(1:12)),
                  c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

months = c(months, months, months, months.leap, months)

#kplant
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[1,3] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[1,7],]
for(i in 1:length(param.keep)){
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

#UptakeRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_UptakeRate)
  MVar_UptakeRate = rbind(MVar_UptakeRate, monthly.avg)
}  

#propN_fol
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_fol)
  MVar_propN_fol = rbind(MVar_propN_fol, monthly.avg)
}  

#propN_roots
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_propN_roots)
  MVar_propN_roots = rbind(MVar_propN_roots, monthly.avg)
}  

#netNrate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_netNrate)
  MVar_netNrate = rbind(MVar_netNrate, monthly.avg)
}  

#cue
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_cue)
  MVar_cue = rbind(MVar_cue, monthly.avg)
}  

#beta
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_beta)
  MVar_beta = rbind(MVar_beta, monthly.avg)
}

#Biomass_C
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[10] = unlist(c(param.keep[i,10]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_BiomassC)
  MVar_BiomassC = rbind(MVar_BiomassC, monthly.avg)
}

#Biomass_N
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[11] = unlist(c(param.keep[i,11]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_BiomassN)
  MVar_BiomassN = rbind(MVar_BiomassN, monthly.avg)
}

#SOM_C
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[12] = unlist(c(param.keep[i,12]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_SOMC)
  MVar_SOMC = rbind(MVar_SOMC, monthly.avg)
}

#SOM_N
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[13] = unlist(c(param.keep[i,13]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_SOMN)
  MVar_SOMN = rbind(MVar_SOMN, monthly.avg)
}

#AvailableN
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[14] = unlist(c(param.keep[i,14]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_AvailN)
  MVar_AvailN = rbind(MVar_AvailN, monthly.avg)
}


MVar_kplant = MVar_kplant[-1,]
MVar_LitterRateC = MVar_LitterRateC[-1,]
MVar_LitterRateN = MVar_LitterRateN[-1,]
MVar_UptakeRate = MVar_UptakeRate[-1,]
MVar_propN_fol = MVar_propN_fol[-1,]
MVar_propN_roots = MVar_propN_roots[-1,]
MVar_netNrate = MVar_netNrate[-1,]
MVar_cue = MVar_cue[-1,]
MVar_beta = MVar_beta[-1,]
MVar_BiomassC = MVar_BiomassC[-1,]
MVar_BiomassN = MVar_BiomassN[-1,]
MVar_SOMC = MVar_SOMC[-1,]
MVar_SOMN = MVar_SOMN[-1,]
MVar_AvailN = MVar_AvailN[-1,]

var.kplant = aggregate(MVar_kplant[,2:11], list(MVar_kplant$Month), var)
var.LitterRateC = aggregate(MVar_LitterRateC[,2:11], list(MVar_LitterRateC$Month), var)
var.LitterRateN = aggregate(MVar_LitterRateN[,2:11], list(MVar_LitterRateN$Month), var)
var.UptakeRate = aggregate(MVar_UptakeRate[,2:11], list(MVar_UptakeRate$Month), var)
var.propN_fol = aggregate(MVar_propN_fol[,2:11], list(MVar_propN_fol$Month), var)
var.propN_roots = aggregate(MVar_propN_roots[,2:11], list(MVar_propN_roots$Month), var)
var.netNrate = aggregate(MVar_netNrate[,2:11], list(MVar_netNrate$Month), var)
var.cue = aggregate(MVar_cue[,2:11], list(MVar_cue$Month), var)
var.beta = aggregate(MVar_beta[,2:11], list(MVar_beta$Month), var)
var.BiomassC = aggregate(MVar_BiomassC[,2:11], list(MVar_BiomassC$Month), var)
var.BiomassN = aggregate(MVar_BiomassN[,2:11], list(MVar_BiomassN$Month), var)
var.SOMC = aggregate(MVar_SOMC[,2:11], list(MVar_SOMC$Month), var)
var.SOMN = aggregate(MVar_SOMN[,2:11], list(MVar_SOMN$Month), var)
var.AvailN = aggregate(MVar_AvailN[,2:11], list(MVar_AvailN$Month), var)

parameters = rep(names(params), c(12,12,12,12,12,12,12,12,12,12,12,12,12,12))

all = rbind(var.kplant, var.LitterRateC, var.LitterRateN, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.netNrate, var.cue, var.beta, var.BiomassC, var.BiomassN,
            var.SOMC, var.SOMN, var.AvailN)

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

perc.UptakeRate = (var.UptakeRate[,2:11]/var.total[,2:11])*100
perc.UptakeRate = cbind(Parameter = rep("UptakeRate", 12), Month=var.total$Group.1, perc.UptakeRate)

perc.propN_fol = (var.propN_fol[,2:11]/var.total[,2:11])*100
perc.propN_fol = cbind(Parameter = rep("propN_fol", 12), Month=var.total$Group.1, perc.propN_fol)

perc.propN_roots = (var.propN_roots[,2:11]/var.total[,2:11])*100
perc.propN_roots = cbind(Parameter = rep("propN_roots", 12), Month=var.total$Group.1, perc.propN_roots)

perc.netNrate = (var.netNrate[,2:11]/var.total[,2:11])*100
perc.netNrate = cbind(Parameter = rep("netNrate", 12), Month=var.total$Group.1, perc.netNrate)

perc.cue = (var.cue[,2:11]/var.total[,2:11])*100
perc.cue = cbind(Parameter = rep("cue", 12), Month=var.total$Group.1, perc.cue)

perc.beta = (var.beta[,2:11]/var.total[,2:11])*100
perc.beta = cbind(Parameter = rep("beta", 12), Month=var.total$Group.1, perc.beta)

perc.BiomassC = (var.BiomassC[,2:11]/var.total[,2:11])*100
perc.BiomassC = cbind(Parameter = rep("BiomassC", 12), Month=var.total$Group.1, perc.BiomassC)

perc.BiomassN = (var.BiomassN[,2:11]/var.total[,2:11])*100
perc.BiomassN = cbind(Parameter = rep("BiomassN", 12), Month=var.total$Group.1, perc.BiomassN)

perc.SOMC = (var.SOMC[,2:11]/var.total[,2:11])*100
perc.SOMC = cbind(Parameter = rep("SOMC", 12), Month=var.total$Group.1, perc.SOMC)

perc.SOMN = (var.SOMN[,2:11]/var.total[,2:11])*100
perc.SOMN = cbind(Parameter = rep("SOMN", 12), Month=var.total$Group.1, perc.SOMN)

perc.AvailN = (var.AvailN[,2:11]/var.total[,2:11])*100
perc.AvailN = cbind(Parameter = rep("AvailN", 12), Month=var.total$Group.1, perc.AvailN)


#create a table binding all together

perc.all = rbind(perc.kplant, perc.LitterRateC, perc.LitterRateN, perc.UptakeRate, 
                 perc.propN_fol, perc.propN_roots, perc.netNrate, perc.cue, perc.beta,
                 perc.BiomassC, perc.BiomassN, perc.SOMC, perc.SOMN, perc.AvailN)
perc.all = perc.all[,-11]
head(perc.all)
tail(perc.all)

####barplots####

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 3:11) { #for each output
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
  sub1[12,] = sub[133:144,3]
  sub1[13,] = sub[145:156,3]
  sub1[14,] = sub[157:168,3]
  barplot(sub1, col=c("darkolivegreen3", "dodgerblue", "aquamarine", "darkgreen", "mediumseagreen",
                      "palegreen", "darkblue", "lightskyblue", "maroon4", "gray87", "azure2", 
                      "gray29", "gray57", "darkslategray"),             
          main=names(perc.all[n]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100)) #plot the data
} #end of for loop


#NDVI
par(mfrow=c(1,1))
sub = perc.all[,c(1,2,11)]
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
sub1[12,] = sub[133:144,3]
sub1[13,] = sub[145:156,3]
sub1[14,] = sub[157:168,3]
barplot(sub1, col=c("darkolivegreen3", "dodgerblue", "aquamarine", "darkgreen", "mediumseagreen",
                    "palegreen", "darkblue", "lightskyblue", "maroon4", "gray87", "azure2", 
                    "gray29", "gray57", "darkslategray"),           
        main=names(perc.all[11]), names.arg=seq(1:12), axisnames=TRUE, ylim=c(0,100), legend=TRUE) #plot the data


####ANNUAL ANALYSIS####
#need to make tables to store annual sums for each model output (each parameter has one table)

AVar_kplant = data.frame(matrix(1,1,3))
colnames(AVar_kplant)=colnames(out[,7:9])
AVar_LitterRateC = data.frame(matrix(1,1,3))
colnames(AVar_LitterRateC)=colnames(out[,7:9])
AVar_LitterRateN = data.frame(matrix(1,1,3))
colnames(AVar_LitterRateN)=colnames(out[,7:9])
AVar_UptakeRate = data.frame(matrix(1,1,3))
colnames(AVar_UptakeRate)=colnames(out[,7:9])
AVar_propN_fol = data.frame(matrix(1,1,3))
colnames(AVar_propN_fol)=colnames(out[,7:9])
AVar_propN_roots = data.frame(matrix(1,1,3))
colnames(AVar_propN_roots)=colnames(out[,7:9])
AVar_netNrate = data.frame(matrix(1,1,3))
colnames(AVar_netNrate)=colnames(out[,7:9])
AVar_cue = data.frame(matrix(1,1,3))
colnames(AVar_cue)=colnames(out[,7:9])
AVar_beta = data.frame(matrix(1,1,3))
colnames(AVar_beta)=colnames(out[,7:9])
AVar_BiomassC = data.frame(matrix(1,1,3))
colnames(AVar_BiomassC)=colnames(out[,7:9])
AVar_BiomassN = data.frame(matrix(1,1,3))
colnames(AVar_BiomassN)=colnames(out[,7:9])
AVar_SOMC = data.frame(matrix(1,1,3))
colnames(AVar_SOMC)=colnames(out[,7:9])
AVar_SOMN = data.frame(matrix(1,1,3))
colnames(AVar_SOMN)=colnames(out[,7:9])
AVar_AvailN = data.frame(matrix(1,1,3))
colnames(AVar_AvailN)=colnames(out[,7:9])

AVar_kplant_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_kplant_NDVI)=c("NDVI")
AVar_LitterRateC_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_LitterRateC_NDVI)=c("NDVI")
AVar_LitterRateN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_LitterRateN_NDVI)=c("NDVI")
AVar_UptakeRate_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_UptakeRate_NDVI)=c("NDVI")
AVar_propN_fol_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_propN_fol_NDVI)=c("NDVI")
AVar_propN_roots_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_propN_roots_NDVI)=c("NDVI")
AVar_netNrate_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_netNrate_NDVI)=c("NDVI")
AVar_cue_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_cue_NDVI)=c("NDVI")
AVar_beta_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_beta_NDVI)=c("NDVI")
AVar_BiomassC_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_BiomassC_NDVI)=c("NDVI")
AVar_BiomassN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_BiomassN_NDVI)=c("NDVI")
AVar_SOMC_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_SOMC_NDVI)=c("NDVI")
AVar_SOMN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_SOMN_NDVI)=c("NDVI")
AVar_AvailN_NDVI = data.frame(matrix(1,1,1))
colnames(AVar_AvailN_NDVI)=c("NDVI")

first=1
second=9

#kplant
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[1,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[1,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[1] = unlist(c(param.keep[i,1]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate avg sum across all years
  names(annual.avg) = names(AVar_kplant) #change names
  AVar_kplant = rbind(AVar_kplant, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_kplant_NDVI) #change names
  AVar_kplant_NDVI = rbind(AVar_kplant_NDVI, annual.avgNDVI) #add row to table
}  

#LitterRateC
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[2,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[2,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_LitterRateC) #change names
  AVar_LitterRateC = rbind(AVar_LitterRateC, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_LitterRateC_NDVI) #change names
  AVar_LitterRateC_NDVI = rbind(AVar_LitterRateC_NDVI, annual.avgNDVI) #add row to table
}  

#LitterRateN
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[3,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[3,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_LitterRateN) #change names
  AVar_LitterRateN = rbind(AVar_LitterRateN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_LitterRateN_NDVI) #change names
  AVar_LitterRateN_NDVI = rbind(AVar_LitterRateN_NDVI, annual.avgNDVI) #add row to table
}  

#UptakeRate
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[4,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[4,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[4] = unlist(c(param.keep[i,4]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_UptakeRate) #change names
  AVar_UptakeRate = rbind(AVar_UptakeRate, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_UptakeRate_NDVI) #change names
  AVar_UptakeRate_NDVI = rbind(AVar_UptakeRate_NDVI, annual.avgNDVI) #add row to table
}  


#propN_fol
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[5,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[5,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[5] = unlist(c(param.keep[i,5]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_propN_fol) #change names
  AVar_propN_fol = rbind(AVar_propN_fol, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_propN_fol_NDVI) #change names
  AVar_propN_fol_NDVI = rbind(AVar_propN_fol_NDVI, annual.avgNDVI) #add row to table
}  

#propN_roots
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[6,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[6,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[6] = unlist(c(param.keep[i,6]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_propN_roots) #change names
  AVar_propN_roots = rbind(AVar_propN_roots, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_propN_roots_NDVI) #change names
  AVar_propN_roots_NDVI = rbind(AVar_propN_roots_NDVI, annual.avgNDVI) #add row to table
}  

#netNrate
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[7,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[7,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[7] = unlist(c(param.keep[i,7]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_netNrate) #change names
  AVar_netNrate = rbind(AVar_netNrate, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_netNrate_NDVI) #change names
  AVar_netNrate_NDVI = rbind(AVar_netNrate_NDVI, annual.avgNDVI) #add row to table
}  

#cue
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[8,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[8,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[8] = unlist(c(param.keep[i,8]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_cue) #change names
  AVar_cue = rbind(AVar_cue, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_cue_NDVI) #change names
  AVar_cue_NDVI = rbind(AVar_cue_NDVI, annual.avgNDVI) #add row to table
}  

#beta
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[9,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[9,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_beta) #change names
  AVar_beta = rbind(AVar_beta, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_beta_NDVI) #change names
  AVar_beta_NDVI = rbind(AVar_beta_NDVI, annual.avgNDVI) #add row to table
} 

#BiomassC
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[10,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[10,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[10] = unlist(c(param.keep[i,10]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_BiomassC) #change names
  AVar_BiomassC = rbind(AVar_BiomassC, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_BiomassC_NDVI) #change names
  AVar_BiomassC_NDVI = rbind(AVar_BiomassC_NDVI, annual.avgNDVI) #add row to table
} 

#BiomassN
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[11,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[11,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[11] = unlist(c(param.keep[i,11]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_BiomassN) #change names
  AVar_BiomassN = rbind(AVar_BiomassN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_BiomassN_NDVI) #change names
  AVar_BiomassN_NDVI = rbind(AVar_BiomassN_NDVI, annual.avgNDVI) #add row to table
} 

#SOMC
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[12,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[12,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[12] = unlist(c(param.keep[i,12]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_SOMC) #change names
  AVar_SOMC = rbind(AVar_SOMC, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_SOMC_NDVI) #change names
  AVar_SOMC_NDVI = rbind(AVar_SOMC_NDVI, annual.avgNDVI) #add row to table
} 

#SOMN
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[13,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[13,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[13] = unlist(c(param.keep[i,13]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_SOMN) #change names
  AVar_SOMN = rbind(AVar_SOMN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_SOMN_NDVI) #change names
  AVar_SOMN_NDVI = rbind(AVar_SOMN_NDVI, annual.avgNDVI) #add row to table
} 

#AvailN
param.keep=param.keep_NEE_NDVI_UNBdata[param.keep_NEE_NDVI_UNBdata$kplant>=summarytable[14,first] & param.keep_NEE_NDVI_UNBdata$kplant<=summarytable[14,second],]
for(i in 1:length(param.keep[,1])){
  params.i = means #set parmeters to mean values
  params.i[14] = unlist(c(param.keep[i,14]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  annual.sum=aggregate(out.i[,7:9], list(out.i$year), sum) #calculate annual sum
  annual.avg=apply(annual.sum[,-1], 2, mean) #calculate sum across all years
  names(annual.avg) = names(AVar_AvailN) #change names
  AVar_AvailN = rbind(AVar_AvailN, annual.avg) #add row to table
  annual.max=aggregate(out.i[,11], list(out.i$year), max) #calculate annual max
  annual.avgNDVI=mean(annual.max[,2]) #calculate avg sum across all years
  names(annual.avgNDVI) = names(AVar_AvailN_NDVI) #change names
  AVar_AvailN_NDVI = rbind(AVar_AvailN_NDVI, annual.avgNDVI) #add row to table
} 


AVar_kplant = AVar_kplant[-1,]
AVar_LitterRateC = AVar_LitterRateC[-1,]
AVar_LitterRateN = AVar_LitterRateN[-1,]
AVar_UptakeRate = AVar_UptakeRate[-1,]
AVar_propN_fol = AVar_propN_fol[-1,]
AVar_propN_roots = AVar_propN_roots[-1,]
AVar_netNrate = AVar_netNrate[-1,]
AVar_cue = AVar_cue[-1,]
AVar_beta = AVar_beta[-1,]
AVar_BiomassC = AVar_BiomassC[-1,]
AVar_BiomassN = AVar_BiomassN[-1,]
AVar_SOMC = AVar_SOMC[-1,]
AVar_SOMN = AVar_SOMN[-1,]
AVar_AvailN = AVar_AvailN[-1,]

var.kplant = apply(AVar_kplant, 2, var)
var.LitterRateC = apply(AVar_LitterRateC, 2, var)
var.LitterRateN = apply(AVar_LitterRateN, 2, var)
var.UptakeRate = apply(AVar_UptakeRate, 2, var)
var.propN_fol = apply(AVar_propN_fol, 2, var)
var.propN_roots = apply(AVar_propN_roots, 2, var)
var.netNrate = apply(AVar_netNrate, 2, var)
var.cue = apply(AVar_cue, 2, var)
var.beta = apply(AVar_beta, 2, var)
var.BiomassC = apply(AVar_BiomassC, 2, var)
var.BiomassN = apply(AVar_BiomassN, 2, var)
var.SOMC = apply(AVar_SOMC, 2, var)
var.SOMN = apply(AVar_SOMN, 2, var)
var.AvailN = apply(AVar_AvailN, 2, var)



AVar_kplant_NDVI = AVar_kplant_NDVI[-1,]
AVar_LitterRateC_NDVI  = AVar_LitterRateC_NDVI[-1,]
AVar_LitterRateN_NDVI  = AVar_LitterRateN_NDVI[-1,]
AVar_UptakeRate_NDVI  = AVar_UptakeRate_NDVI[-1,]
AVar_propN_fol_NDVI  = AVar_propN_fol_NDVI[-1,]
AVar_propN_roots_NDVI  = AVar_propN_roots_NDVI[-1,]
AVar_netNrate_NDVI  = AVar_netNrate_NDVI[-1,]
AVar_cue_NDVI  = AVar_cue_NDVI[-1,]
AVar_beta_NDVI  = AVar_beta_NDVI[-1,]
AVar_BiomassC_NDVI  = AVar_BiomassC_NDVI[-1,]
AVar_BiomassN_NDVI  = AVar_BiomassN_NDVI[-1,]
AVar_SOMC_NDVI  = AVar_SOMC_NDVI[-1,]
AVar_SOMN_NDVI  = AVar_SOMN_NDVI[-1,]
AVar_AvailN_NDVI  = AVar_AvailN_NDVI[-1,]

var.kplant_NDVI = var(AVar_kplant_NDVI)
var.LitterRateC_NDVI = var(AVar_LitterRateC_NDVI)
var.LitterRateN_NDVI = var(AVar_LitterRateN_NDVI)
var.UptakeRate_NDVI = var(AVar_UptakeRate_NDVI)
var.propN_fol_NDVI = var(AVar_propN_fol_NDVI)
var.propN_roots_NDVI = var(AVar_propN_roots_NDVI)
var.netNrate_NDVI = var(AVar_netNrate_NDVI)
var.cue_NDVI = var(AVar_cue_NDVI)
var.beta_NDVI = var(AVar_beta_NDVI)
var.BiomassC_NDVI = var(AVar_BiomassC_NDVI)
var.BiomassN_NDVI = var(AVar_BiomassN_NDVI)
var.SOMC_NDVI = var(AVar_SOMC_NDVI)
var.SOMN_NDVI = var(AVar_SOMN_NDVI)
var.AvailN_NDVI = var(AVar_AvailN_NDVI)


all_NEE = rbind(var.kplant, var.LitterRateC, var.LitterRateN, var.UptakeRate, var.propN_fol, 
            var.propN_roots, var.netNrate, var.cue, var.beta, var.BiomassC, var.BiomassN, 
            var.SOMC, var.SOMN, var.AvailN)
all_NDVI = rbind(var.kplant_NDVI, var.LitterRateC_NDVI, var.LitterRateN_NDVI, var.UptakeRate_NDVI, var.propN_fol_NDVI, 
                var.propN_roots_NDVI, var.netNrate_NDVI, var.cue_NDVI, var.beta_NDVI, var.BiomassC_NDVI, var.BiomassN_NDVI, 
                var.SOMC_NDVI, var.SOMN_NDVI, var.AvailN_NDVI)

rownames(all_NEE)=names(params)
rownames(all_NDVI)=names(params)

#calculate total variance
var.total = apply(all_NEE, 2, sum)
var.total_NDVI = apply(all_NDVI, 2, sum)

#now calculate percent variance
perc.kplant = (var.kplant/var.total)*100
perc.LitterRateC = (var.LitterRateC/var.total)*100
perc.LitterRateN = (var.LitterRateN/var.total)*100
perc.UptakeRate = (var.UptakeRate/var.total)*100
perc.propN_fol = (var.propN_fol/var.total)*100
perc.propN_roots = (var.propN_roots/var.total)*100
perc.netNrate = (var.netNrate/var.total)*100
perc.cue = (var.cue/var.total)*100
perc.beta = (var.beta/var.total)*100

perc.kplant_NDVI = (var.kplant_NDVI/var.total_NDVI)*100
perc.LitterRateC_NDVI = (var.LitterRateC_NDVI/var.total_NDVI)*100
perc.LitterRateN_NDVI = (var.LitterRateN_NDVI/var.total_NDVI)*100
perc.UptakeRate_NDVI = (var.UptakeRate_NDVI/var.total_NDVI)*100
perc.propN_fol_NDVI = (var.propN_fol_NDVI/var.total_NDVI)*100
perc.propN_roots_NDVI = (var.propN_roots_NDVI/var.total_NDVI)*100
perc.netNrate_NDVI = (var.netNrate_NDVI/var.total_NDVI)*100
perc.cue_NDVI = (var.cue_NDVI/var.total_NDVI)*100
perc.beta_NDVI = (var.beta_NDVI/var.total_NDVI)*100

#create a table binding all together

perc.all_NEE = rbind(perc.kplant, perc.LitterRateC, perc.LitterRateN, perc.UptakeRate, 
                   perc.propN_fol, perc.propN_roots, perc.netNrate, perc.cue, perc.beta, var.total)

rownames(perc.all_NEE)=names(params[1:9])

perc.all_NDVI = rbind(perc.kplant_NDVI, perc.LitterRateC_NDVI, perc.LitterRateN_NDVI, perc.UptakeRate_NDVI, 
                     perc.propN_fol_NDVI, perc.propN_roots_NDVI, perc.netNrate_NDVI, perc.cue_NDVI, perc.beta_NDVI, var.total_NDVI)

rownames(perc.all_NDVI)=names(params[1:9])
colnames(perc.all.NDVI)=c("NDVI")



#store for this subset
perc.all.NEE_90 = data.frame(perc.all_NEE)
perc.all.NDVI_90 = data.frame(perc.all_NDVI)

####barplots####
barplot(perc.all_NEE, col=c("chartreuse", "cadetblue", "aquamarine", "darkblue",  "purple", 
                          "deepskyblue", "dodgerblue3", "forestgreen", "darkgray"),  legend=TRUE )
barplot(perc.all.NEE$NEE, names.arg=names(params[1:9]), cex.names=0.5, 
        col="forestgreen", horiz=TRUE, main="NEE") #plot the data

barplot(perc.all.NDVI$NDVI, names.arg=names(params[1:9]), cex.names=0.5, 
        col="forestgreen", horiz=TRUE, main="NDVI") #plot the data

save.image(file="Variance_120315_3.Rdata")

