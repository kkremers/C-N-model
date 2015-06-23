#in this script, the output from all of the synthetic data experiments is compiled so that it can be compared

#first, need to create a table to store the output
summarytable = data.frame(matrix(1,63,8)) 
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


load("Step2_NEE_NDVI.Rdata")
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
write.csv(param.keep_2, "Params_NEE_NDVI.csv")


load("Step2_NEE_GPP_Re.Rdata")
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
write.csv(param.keep_3, "Params_NEE_GPP_Re.csv")



load("Step2_NEE_GPP_Re_NDVI.Rdata")
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
write.csv(param.keep_4, "Params_NEE_GPP_Re_NDVI.csv")

load("Step2_NEE_BiomassCN.Rdata")
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_5 = param.best
exper = rep(5, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[37:45,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_5 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_5, "Params_NEE_BiomassCN.csv")


load("Step2_NEE_BiomassCN_AvailableN.Rdata")
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_6 = param.best
exper = rep(6, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[46:54,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_6 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_6, "Params_NEE_BiomassCN_AvailableN.csv")


load("Step2_NEE_BiomassCN_SOMCN_AvailableN.Rdata")
q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
diff=param.best-params
param.best_7 = param.best
exper = rep(7, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[55:63,]=cbind(exper, parameters, q05, q25, means, q75, q95, diff) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_7 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_7, "Params_NEE_BiomassCN_SOMCN_AvailableN.csv")


#preview table
head(summarytable)
tail(summarytable)

#create table of param.best
best.params = data.frame(matrix(1, 7, 9))
colnames(best.params)=names(param.best)
best.params[1,] = param.best_1
best.params[2,] = param.best_2
best.params[3,] = param.best_3
best.params[4,] = param.best_4
best.params[5,] = param.best_5
best.params[6,] = param.best_6
best.params[7,] = param.best_7
best.params

#need to know value of expected parameters
params <- c(kplant = 2,
            LitterRate_S = 0.0008,
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
  box.dat = matrix(1, 5, 7)
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



out=data.frame(solvemodel(params, state))[,c(2:9,11,12,13)] #with columns to match data.assim
out1=data.frame(solvemodel(param.best_1, state))[,c(2:9,11,12,13)] 
out2=data.frame(solvemodel(param.best_2, state))[,c(2:9,11,12,13)]
out3=data.frame(solvemodel(param.best_3, state))[,c(2:9,11,12,13)]
out4=data.frame(solvemodel(param.best_4, state))[,c(2:9,11,12,13)]
out5=data.frame(solvemodel(param.best_5, state))[,c(2:9,11,12,13)]
out6=data.frame(solvemodel(param.best_6, state))[,c(2:9,11,12,13)]
out7=data.frame(solvemodel(param.best_7, state))[,c(2:9,11,12,13)]

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
reg_7 = lm(out[,7]~out1[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out1[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out1[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out1[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out1[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared1 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

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
reg_7 = lm(out[,7]~out2[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out2[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out2[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out2[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out2[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared2 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

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
reg_7 = lm(out[,7]~out3[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out3[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out3[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out3[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out3[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared3 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

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
reg_7 = lm(out[,7]~out4[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out4[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out4[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out4[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out4[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared4 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

#########EXPERIMENT 5###########
#calculate RMSE
error = (out-out5)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE5 = sqrt(mean)
#calculate MAE
abs.error = abs(out5-out)
MAE5 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out5[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out5[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out5[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out5[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out5[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out5[,6])
r2_6 = summary(reg_6)$r.squared
reg_7 = lm(out[,7]~out5[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out5[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out5[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out5[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out5[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared5 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

########EXPERIMENT 6##########
#calculate RMSE
error = (out-out6)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE6 = sqrt(mean)
#calculate MAE
abs.error = abs(out6-out)
MAE6 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out6[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out6[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out6[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out6[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out6[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out6[,6])
r2_6 = summary(reg_6)$r.squared
reg_7 = lm(out[,7]~out6[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out6[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out6[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out6[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out6[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared6 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

##########EXPERIMENT 7##########
#calculate RMSE
error = (out-out7)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE7 = sqrt(mean)
#calculate MAE
abs.error = abs(out7-out)
MAE7 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(out[,1]~out7[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(out[,2]~out7[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(out[,3]~out7[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(out[,4]~out7[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(out[,5]~out7[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(out[,6]~out7[,6])
r2_6 = summary(reg_6)$r.squared
reg_7 = lm(out[,7]~out7[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(out[,8]~out7[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(out[,9]~out7[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(out[,10]~out7[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(out[,11]~out7[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared7 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

###############


#now add them all to the comparison table
modelcompare = data.frame(matrix(1,11,22))
colnames(modelcompare) = c("Output", 
                           "r2_1", "MAE_1", "RMSE_1", 
                           "r2_2", "MAE_2", "RMSE_2",
                           "r2_3", "MAE_3", "RMSE_3",
                           "r2_4", "MAE_4", "RMSE_4",
                           "r2_5", "MAE_5", "RMSE_5",
                           "r2_6", "MAE_6", "RMSE_6",
                           "r2_7", "MAE_7", "RMSE_7")

modelcompare$Output = colnames(out)
modelcompare$r2_1 = rsquared1
modelcompare$r2_2 = rsquared2
modelcompare$r2_3 = rsquared3
modelcompare$r2_4 = rsquared4
modelcompare$r2_5 = rsquared5
modelcompare$r2_6 = rsquared6
modelcompare$r2_7 = rsquared7
modelcompare$MAE_1 = MAE1
modelcompare$MAE_2 = MAE2
modelcompare$MAE_3 = MAE3
modelcompare$MAE_4 = MAE4
modelcompare$MAE_5 = MAE5
modelcompare$MAE_6 = MAE6
modelcompare$MAE_7 = MAE7
modelcompare$RMSE_1 = RMSE1
modelcompare$RMSE_2 = RMSE2
modelcompare$RMSE_3 = RMSE3
modelcompare$RMSE_4 = RMSE5
modelcompare$RMSE_5 = RMSE5
modelcompare$RMSE_6 = RMSE6
modelcompare$RMSE_7 = RMSE7

head(modelcompare)

#save table as CSV

write.csv(modelcompare, "ModelCompare.csv")





#######plots of linear regressions#######
par(mfrow=c(4,2), mar=c(4,4,2,2))

plot(out[,8], out1[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out1[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out1[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out1[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out1[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out1[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out1[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out1[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")





plot(out[,8], out2[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out2[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out2[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out2[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out2[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out2[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out2[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out2[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")





plot(out[,8], out3[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out3[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out3[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out3[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out3[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out3[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out3[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out3[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")





plot(out[,8], out4[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out4[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out4[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out4[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out4[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out4[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out4[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out4[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")





plot(out[,8], out5[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out5[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out5[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out5[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out5[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out5[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out5[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out5[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")





plot(out[,8], out6[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out6[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out6[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out6[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out6[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out6[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out6[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out6[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,8], out7[,8], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(0,1,col="red")
reg1=lm(out7[,8]~out[,8])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,10], out7[,10], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(0,1,col="red")
reg1=lm(out7[,10]~out[,10])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,11], out7[,11], xlab= "Actual", ylab="Modelled", main = "Re")
abline(0,1,col="red")
reg1=lm(out7[,11]~out[,11])
plot(density(resid(reg1)), main="Density of Residuals")

plot(out[,9], out7[,9], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(0,1,col="red")
reg1=lm(out7[,9]~out[,9])
plot(density(resid(reg1)), main="Density of Residuals")