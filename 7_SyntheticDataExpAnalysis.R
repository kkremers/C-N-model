#in this script, the output from all of the synthetic data experiments is compiled so that it can be compared

#first, need to create a table to store the output
summarytable = data.frame(matrix(1,36,8)) 
colnames(summarytable) = c("Experiment", "Parameter", "q05", "q25", "mean", "q75", "q95", "diff")
head(summarytable)


#Now, load each workspace and save the summary statistics to summary table
load("Step2_NEE.Rdata") #load workspace
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_1 = param.best
exper = rep(1, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[1:9,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_1 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_1, "Params_NEE.csv")


load("Step2_NEE_BiomassCN.Rdata")
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_2 = param.best
exper = rep(2, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[10:18,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_2 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_2, "Params_NEE_BiomassCN.csv")


load("Step2_NEE_BiomassCN_AvailableN.Rdata")
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_3 = param.best
exper = rep(3, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[19:27,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_3 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_3, "Params_NEE_BiomassCN_AvailableN.csv")


load("Step2_NEE_BiomassCN_AvailableN_SOMCN.Rdata")
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_4 = param.best
exper = rep(4, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[28:36,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_4 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_4, "Params_NEE_BiomassCN_AvailableN_SOMCN.csv")


#preview table
head(summarytable)
tail(summarytable)

#create table of param.best
best.params = data.frame(matrix(1, 4, 9))
colnames(best.params)=names(param.best)
best.params[1,] = param.best_1
best.params[2,] = param.best_2
best.params[3,] = param.best_3
best.params[4,] = param.best_4
best.params

#need to know value of expected parameters
params <- c(kplant = 2,
            LitterRate = 0.0008,
            retrans = 0.85,  
            RespRate = 0.9, 
            UptakeRate = 0.01,
            propN_fol = 0.3,
            propN_roots = 0.5,
            q10 = 2,
            Ndep_rate = 0.0005
)


####create boxplots#####

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 1:n.param) { #for each parameter
  
  dat = summarytable[which(summarytable$Parameter==names(params[n])),3:7] #pull out data for that parameter
  box.dat = matrix(1, 5, 4)
  box.dat[1,]=as.numeric(dat[,1])
  box.dat[2,]=as.numeric(dat[,2])
  box.dat[3,]=as.numeric(dat[,3])
  box.dat[4,]=as.numeric(dat[,4])
  box.dat[5,]=as.numeric(dat[,5])
  boxplot(box.dat, col=c("palegreen", "darkslategray", "cadetblue", "aquamarine4", "lightcyan2", "palegreen4", "lightblue"), main=names(params[n])) #plot the data
  abline(h=as.numeric(params[n]), col="black", lty=2) #add line where expected parameter value is
  points(best.params[,n], col="red", cex=1.5, pch=16)
} #end of for loop


####create bargraphs#####

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 1:n.param) { #for each parameter
  
  dat = as.vector(as.numeric((summarytable[which(summarytable$Parameter==names(params[n])),8]))) #pull out data for that parameter
  barplot(dat, col=c("palegreen", "darkslategray", "cadetblue", "aquamarine4", "lightcyan2", "palegreen4", "lightblue"), main=names(params[n])) #plot the data
  abline(h=0, col="black") #add line where expected parameter value is
} #end of for loop




####statistical calculations####
state <- c(Biomass_C = 400, 
           Biomass_N = 4.5, 
           SOM_C = 1600, 
           SOM_N = 35,
           Available_N = 0.1)



out=data.frame(solvemodel(params, state))[,c(2:7)] #with columns to match data.assim
out1=data.frame(solvemodel(param.best_1, state))[,c(2:7)] 
out2=data.frame(solvemodel(param.best_2, state))[,c(2:7)]
out3=data.frame(solvemodel(param.best_3, state))[,c(2:7)]
out4=data.frame(solvemodel(param.best_4, state))[,c(2:7)]


head(out1)
head(out)
#make sure columns of the above tables match


#now, for each experiment (1-7), calculate bias mean error, MAE, and R2 for each stock/flux of interest

#######EXPERIMENT 1########
#calculate RMSE
error = (out-out1)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE1 = sqrt(mean)
#calculate MAE
abs.error = abs(out1-out)
MAE1 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out1[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out1[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out1[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out1[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out1[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out1[,6])
r2_6 = summary(reg_6)$r.squared


#put all of them into one vector
rsquared1 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6)

########EXPERIMENT 2#######
#calculate RMSE
error = (out-out2)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE2 = sqrt(mean)
#calculate MAE
abs.error = abs(out2-out)
MAE2 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out2[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out2[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out2[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out2[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out2[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out2[,6])
r2_6 = summary(reg_6)$r.squared

#put all of them into one vector
rsquared2 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6)

########EXPERIMENT 3############
#calculate RMSE
error = (out-out3)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE3 = sqrt(mean)
#calculate MAE
abs.error = abs(out3-out)
MAE3 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out3[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out3[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out3[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out3[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out3[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out3[,6])
r2_6 = summary(reg_6)$r.squared


#put all of them into one vector
rsquared3 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6)

#########EXPERIMENT 4############
#calculate RMSE
error = (out-out4)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE4 = sqrt(mean)
#calculate MAE
abs.error = abs(out4-out)
MAE4 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out4[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out4[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out4[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out4[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out4[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out4[,6])
r2_6 = summary(reg_6)$r.squared


#put all of them into one vector
rsquared4 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6)

###############


#now add them all to the comparison table
modelcompare = data.frame(matrix(1,6,13))
colnames(modelcompare) = c("Output", 
                           "r2_1", "MAE_1", "RMSE_1", 
                           "r2_2", "MAE_2", "RMSE_2",
                           "r2_3", "MAE_3", "RMSE_3",
                           "r2_4", "MAE_4", "RMSE_4")

modelcompare$Output = colnames(out)
modelcompare$r2_1 = rsquared1
modelcompare$r2_2 = rsquared2
modelcompare$r2_3 = rsquared3
modelcompare$r2_4 = rsquared4
modelcompare$MAE_1 = MAE1
modelcompare$MAE_2 = MAE2
modelcompare$MAE_3 = MAE3
modelcompare$MAE_4 = MAE4
modelcompare$RMSE_1 = RMSE1
modelcompare$RMSE_2 = RMSE2
modelcompare$RMSE_3 = RMSE3
modelcompare$RMSE_4 = RMSE4

head(modelcompare)

#save table as CSV

write.csv(modelcompare, "ModelCompare.csv")


#######plots of linear regressions#######
par(mfrow=c(4,2), mar=c(4,4,2,2))

plot(out[,6], out1[,6], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out1[,6]~out[,6])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,6], out1[,6], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg2=lm(out2[,6]~out[,6])
plot(density(resid(reg2)), main="Density of Residuals")

plot(out[,6], out1[,6], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg3=lm(out3[,6]~out[,6])
plot(density(resid(reg3)), main="Density of Residuals")

plot(out[,6], out1[,6], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg4=lm(out4[,6]~out[,6])
plot(density(resid(reg4)), main="Density of Residuals")



######VARIANCE DECOMPOSITION ANALYSIS#########

load("Step2_NEE_BiomassCN_AvailableN_SOMCN.Rdata") #load best experiment (experiment 4)
head(param.keep) #view table of accepted parameters
means=apply(param.keep, 2, mean) #calculate parameter means

#to perform the variance decomposition analysis, you need to:
# 1) alter each parameter individually holding all other parameters constant at their means
# 2) run the model for each parameter set to obtain an ensemble of model runs
# 3) for each model run, calculate the monthly average of the output
# 4) Calculate the variance in monthly averages for each parameter - this gives you the contribution of that parameter to the model variance

head(out[,1:11])
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
MVar_Ndep_rate = data.frame(matrix(1,1,11))
colnames(MVar_Ndep_rate)=c("Month", colnames(out[,2:11]))


#need to create a vector of months to append to model output
months = rep(c("1_Jan", "2_Feb", "3_Mar", "4_Apr", "5_May", "6_Jun", "7_Jul", "8_Aug", "9_Sept", "10_Oct", "11_Nov", "12_Dec"),
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
months.leap = rep(c("1_Jan", "2_Feb", "3_Mar", "4_Apr", "5_May", "6_Jun", "7_Jul", "8_Aug", "9_Sept", "10_Oct", "11_Nov", "12_Dec"),
                  c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

months = c(months, months, months, months.leap, months)

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

#LitterRate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[2] = unlist(c(param.keep[i,2]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_LitterRate)
  MVar_LitterRate = rbind(MVar_LitterRate, monthly.avg)
}  

#retrans
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[3] = unlist(c(param.keep[i,3]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_retrans)
  MVar_retrans = rbind(MVar_retrans, monthly.avg)
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

#Ndep_rate
for(i in 1:1000){
  params.i = means #set parmeters to mean values
  params.i[9] = unlist(c(param.keep[i,9]))  #change the parameter value of interest
  out.i = data.frame(solvemodel(params.i, state)) #run model
  out.i = cbind(out.i, Month = months) #add month vector
  monthly.avg=aggregate(out.i[,2:11], list(out.i$Month), mean)
  names(monthly.avg) = names(MVar_Ndep_rate)
  MVar_Ndep_rate = rbind(MVar_Ndep_rate, monthly.avg)
}  

MVar_kplant = MVar_kplant[-1,]
MVar_LitterRate = MVar_LitterRate[-1,]
MVar_retrans = MVar_retrans[-1,]
MVar_RespRate = MVar_RespRate[-1,]
MVar_UptakeRate = MVar_UptakeRate[-1,]
MVar_propN_fol = MVar_propN_fol[-1,]
MVar_propN_roots = MVar_propN_roots[-1,]
MVar_q10 = MVar_q10[-1,]
MVar_Ndep_rate = MVar_Ndep_rate[-1,]

var.kplant = aggregate(MVar_kplant[,2:11], list(MVar_kplant$Month), var)
var.LitterRate = aggregate(MVar_LitterRate[,2:11], list(MVar_LitterRate$Month), var)
var.retrans = aggregate(MVar_retrans[,2:11], list(MVar_retrans$Month), var)
var.RespRate = aggregate(MVar_RespRate[,2:11], list(MVar_RespRate$Month), var)
var.UptakeRate = aggregate(MVar_UptakeRate[,2:11], list(MVar_UptakeRate$Month), var)
var.propN_fol = aggregate(MVar_propN_fol[,2:11], list(MVar_propN_fol$Month), var)
var.propN_roots = aggregate(MVar_propN_roots[,2:11], list(MVar_propN_roots$Month), var)
var.q10 = aggregate(MVar_q10[,2:11], list(MVar_q10$Month), var)
var.Ndep_rate = aggregate(MVar_Ndep_rate[,2:11], list(MVar_Ndep_rate$Month), var)

save.image(file="Variance_07012015.Rdata")





