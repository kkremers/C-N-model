#in this script, the output from all of the synthetic data experiments is compiled so that it can be compared

#first, need to create a table to store the output
summarytable = data.frame(matrix(1,54,7)) 
colnames(summarytable) = c("Experiment", "Parameter", "min", "q25", "mean", "q75", "max")
head(summarytable)


#Now, load each workspace and save the summary statistics to summary table
load("Step2_NEE.Rdata") #load workspace
min=apply(param.keep, 2, min) #calculate min value accepted
q25=apply(param.keep, 2, quantile, 0.25) #calculate lower quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate upper quantile
max=apply(param.keep, 2, max) #calculate max value accepted
param.best_1 = param.best
exper = rep(1, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[1:9,]=cbind(exper, parameters, min, q25, means, q75, max) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_1 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_1, "Params_NEE.csv")


load("Step2_NEE_NDVI.Rdata")
min=apply(param.keep, 2, min) #calculate min value accepted
q25=apply(param.keep, 2, quantile, 0.25) #calculate lower quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate upper quantile
max=apply(param.keep, 2, max) #calculate max value accepted
param.best_2 = param.best
exper = rep(2, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[10:18,]=cbind(exper, parameters, min, q25, means, q75, max) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_2 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_2, "Params_NEE_NDVI.csv")


load("Step2_NEE_GPP_Re.Rdata")
min=apply(param.keep, 2, min) #calculate min value accepted
q25=apply(param.keep, 2, quantile, 0.25) #calculate lower quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate upper quantile
max=apply(param.keep, 2, max) #calculate max value accepted
param.best_3 = param.best
exper = rep(3, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[19:27,]=cbind(exper, parameters, min, q25, means, q75, max) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_3 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_3, "Params_NEE_GPP_Re.csv")


load("Step2_NEE_BiomassCN.Rdata")
min=apply(param.keep, 2, min) #calculate min value accepted
q25=apply(param.keep, 2, quantile, 0.25) #calculate lower quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate upper quantile
max=apply(param.keep, 2, max) #calculate max value accepted
param.best_4 = param.best
exper = rep(4, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[28:36,]=cbind(exper, parameters, min, q25, means, q75, max) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_4 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_4, "Params_NEE_BiomassCN.csv")


load("Step2_NEE_BiomassCN_AvailableN.Rdata")
min=apply(param.keep, 2, min) #calculate min value accepted
q25=apply(param.keep, 2, quantile, 0.25) #calculate lower quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate upper quantile
max=apply(param.keep, 2, max) #calculate max value accepted
param.best_5 = param.best
exper = rep(5, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[37:45,]=cbind(exper, parameters, min, q25, means, q75, max) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_5 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_5, "Params_NEE_BiomassCN_AvailableN.csv")


load("Step2_NEE_BiomassCN_SOMCN_AvailableN.Rdata")
min=apply(param.keep, 2, min) #calculate min value accepted
q25=apply(param.keep, 2, quantile, 0.25) #calculate lower quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate upper quantile
max=apply(param.keep, 2, max) #calculate max value accepted
param.best_6 = param.best
exper = rep(6, n.param) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
summarytable[46:54,]=cbind(exper, parameters, min, q25, means, q75, max) #bind all of the information together in the proper order (same order as summarytable columns)
param.keep_6 = param.keep #save the table of accepted parameters under a new name
write.csv(param.keep_6, "Params_NEE_BiomassCN_SOMCN_AvailableN.csv")


#preview table
head(summarytable)
tail(summarytable)

#create table of param.best
best.params = data.frame(matrix(1, 6, 9))
colnames(best.params)=names(param.best)
best.params[1,] = param.best_1
best.params[2,] = param.best_2
best.params[3,] = param.best_3
best.params[4,] = param.best_4
best.params[5,] = param.best_5
best.params[6,] = param.best_6
best.params

#need to know value of expected parameters
params <- c(kplant = 0.11,
            LitterRate = 0.0025,
            DecompRateC = 0.005,
            DecompRateN = 0.0007,
            retrans = 0.8,  
            RespRate = 1, 
            UptakeRate = 0.0001,
            netNrate = 0.0008,
            q10 = 2)


######create boxplots#####

par(mfrow=c(3,3), mar=c(4,4,2,2))

for (n in 1:n.param) { #for each parameter
  
  dat = summarytable[which(summarytable$Parameter==names(params[n])),3:7] #pull out data for that parameter
  box.dat = matrix(1, 5, 6)
  box.dat[1,]=as.numeric(dat[,1])
  box.dat[2,]=as.numeric(dat[,2])
  box.dat[3,]=as.numeric(dat[,3])
  box.dat[4,]=as.numeric(dat[,4])
  box.dat[5,]=as.numeric(dat[,5])
  boxplot(box.dat, col=c("palegreen", "darkslategray", "cadetblue", "aquamarine4", "lightcyan2", "palegreen4"), main=names(params[n])) #plot the data
  abline(h=as.numeric(params[n]), col="black", lty=2) #add line where expected parameter value is
  points(best.params[,n], col="red", cex=1.5, pch=16)
} #end of for loop


####statistical calculations####
state <- c(Biomass_C = 400, 
           Biomass_N = 4.5, 
           Litter_C = 160, 
           Litter_N = 1.6, 
           SOM_C = 2000, 
           SOM_N = 56,
           Available_N = 0.1)

head(data.assim) #may need to re-run param optimization step 1 code to get this (will need to re-run model with params used to create assimilation data)
#remove time column
data.assim = data.assim[,-1]

out1=data.frame(solvemodel(param.best_1, state))[,c(2:9,11,12,13)] #with columns to match data.assim
out2=data.frame(solvemodel(param.best_2, state))[,c(2:9,11,12,13)]
out3=data.frame(solvemodel(param.best_3, state))[,c(2:9,11,12,13)]
out4=data.frame(solvemodel(param.best_4, state))[,c(2:9,11,12,13)]
out5=data.frame(solvemodel(param.best_5, state))[,c(2:9,11,12,13)]
out6=data.frame(solvemodel(param.best_6, state))[,c(2:9,11,12,13)]

head(out1)
head(data.assim)
#make sure columns of the above tables match


#now, for each experiment (1-6), calculate bias mean error, MAE, and R2 for each stock/flux of interest

#calculate RMSE
error = (data.assim-out1)
errorsquared = error^2
mean = apply(errorsquared, 2, mean, na.rm=TRUE)
RMSE1 = sqrt(mean)
#calculate MAE
abs.error = abs(out1-data.assim)
MAE1 = apply(abs.error, 2, mean, na.rm=TRUE)
#calculate r2
reg_1 = lm(data.assim[,1]~out1[,1])
r2_1 = summary(reg_1)$r.squared
reg_2 = lm(data.assim[,2]~out1[,2])
r2_2 = summary(reg_2)$r.squared
reg_3 = lm(data.assim[,3]~out1[,3])
r2_3 = summary(reg_3)$r.squared
reg_4 = lm(data.assim[,4]~out1[,4])
r2_4 = summary(reg_4)$r.squared
reg_5 = lm(data.assim[,5]~out1[,5])
r2_5 = summary(reg_5)$r.squared
reg_6 = lm(data.assim[,6]~out1[,6])
r2_6 = summary(reg_6)$r.squared
reg_7 = lm(data.assim[,7]~out1[,7])
r2_7 = summary(reg_7)$r.squared
reg_8 = lm(data.assim[,8]~out1[,8])
r2_8 = summary(reg_8)$r.squared
reg_9 = lm(data.assim[,9]~out1[,9])
r2_9 = summary(reg_9)$r.squared
reg_10 = lm(data.assim[,10]~out1[,10])
r2_10 = summary(reg_10)$r.squared
reg_11 = lm(data.assim[,11]~out1[,11])
r2_11 = summary(reg_11)$r.squared

#put all of them into one vector
rsquared1 = c(r2_1, r2_2, r2_3, r2_4, r2_5, r2_6, r2_7, r2_8, r2_9, r2_10, r2_11)

#now add them all to the comparison table
modelcompare = data.frame(matrix(1,11,19))
colnames(modelcompare) = c("Output", 
                           "r2_1", "MAE_1", "RMSE_1", 
                           "r2_2", "MAE_2", "RMSE_2",
                           "r2_3", "MAE_3", "RMSE_3",
                           "r2_4", "MAE_4", "RMSE_4",
                           "r2_5", "MAE_5", "RMSE_5",
                           "r2_6", "MAE_6", "RMSE_6")

modelcompare$Output = colnames(data.assim)
modelcompare$r2_1 = rsquared1
modelcompare$r2_2 = rsquared2
modelcompare$r2_3 = rsquared3
modelcompare$r2_4 = rsquared4
modelcompare$r2_5 = rsquared5
modelcompare$r2_6 = rsquared6
modelcompare$MAE_1 = MAE1
modelcompare$MAE_2 = MAE2
modelcompare$MAE_3 = MAE3
modelcompare$MAE_4 = MAE4
modelcompare$MAE_5 = MAE5
modelcompare$MAE_6 = MAE6
modelcompare$RMSE_1 = RMSE1
modelcompare$RMSE_2 = RMSE2
modelcompare$RMSE_3 = RMSE3
modelcompare$RMSE_4 = RMSE5
modelcompare$RMSE_5 = RMSE5
modelcompare$RMSE_6 = RMSE6

head(modelcompare)

#save table as CSV

write.csv(modelcompare, "ModelCompare.csv")