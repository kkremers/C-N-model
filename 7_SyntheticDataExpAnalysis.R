#in this script, the output from all of the synthetic data experiments is compiled so that it can be compared

#first, need to create a table to store the output
summarytable = NULL 
colnames(summarytable) = c("Experiment", "Parameter", "q05", "lowerbox", "mean", "upperbox", "q95")


#Now, load each workspace and save the summary statistics to summary table
load("Step2_NEE.Rdata") #load workspace
means=apply(param.keep, 2, mean) #calculate parameter means
sds=apply(param.keep, 2, sd) #calculate parameter standard deviations
q05=apply(param.keep, 2, quantile, 0.05) #calculate lower quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate upper quantile
lowerbox=means-sds #calculate where lower edge of box should be
upperbox=means+sds #calculate where upper edge of box should be
exper = rep("NEE", n.params) #create vector of experiment name
parameters = names(param.best) #create vector of parameter names
stats=cbind(exper, parameters, q05, lowerbox, means, upperbox, q95) #bind all of the information together in the proper order (same order as summarytable columns)
rbind(summarytable, stats) #add these rows to the summary table
param.keep_NEE = param.keep #save the table of accepted parameters under a new name


load("Step2_NEE_NDVI.Rdata")


load("Step2_NEE_GPP_Re.Rdata")


load("Step2_NEE_BiomassCN.Rdata")


load("Step2_NEE_BiomassCN_AvailableN.Rdata")


load("Step2_NEE_BiomassCN_SOMCN_AvailableN.Rdata")




#preview table
head(summarytable)
tail(summarytable)

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

#create vector of colors (one color for each parameter)
colors=c("aquamarine4", "lightcyan2", "palegreen3", "cadetblue", "darkslategray4", "aquamarine", "lightseagreen", "cadetblue2", "darkseagreen2")

par(mfrow=c(3,3)) #set so that plots are 3x3

for (n in 1:n.params) { #for each parameter
  
  dat = summarytable[which(summarytable$Parameters==names(params[n])),] #pull out data for that parameter
  bxp(dat, col=colors[n], main=names(params[n])) #plot the data
  abline(h=as.numeric(params[n]), col="gray", lty=2, lwd=2) #add line where expected parameter value is
  
} #end of for loop











