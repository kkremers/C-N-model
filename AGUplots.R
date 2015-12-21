############PLOT ASSIMILATION DATA###########
data.assim = read.csv("Assimilation_data_all.csv")
data.sigma = read.csv("Assimilation_sigma_all.csv")
data.assim = data.assim[data.assim$Year != 2013,]
data.sigma = data.sigma[data.sigma$Year != 2013,]
head(data.assim)
head(data.sigma)
tail(data.assim)
tail(data.sigma)
out=data.frame(solvemodel(param.best)) #with columns to match data.assim
head(out)
out1=cbind(out, year_DOY=interaction(out$year, out$DOY, sep="_"))
head(out1)
time.assim = out1[match(data.assim$Year_DOY, out1$year_DOY), 1]
data.compare1=data.frame(cbind(time=time.assim, NEE=data.assim[,6], NDVI=data.assim[,9]))
sigma.obs1 = data.frame(cbind(time=time.assim, NEE=data.sigma[,6], NDVI=data.sigma[,9]))
head(data.compare1)
head(sigma.obs1)
upper=data.compare1+sigma.obs1
lower=data.compare1-sigma.obs1
NEE_dat = data.frame(Time=data.compare1$time, NEE=data.compare1$NEE, lower=lower$NEE, upper=upper$NEE)
head(NEE_dat)
NEE_dat=NEE_dat[!is.na(NEE_dat$NEE),]
head(NEE_dat)
NEE_dat = data.frame(Time=data.compare1$time, NEE=data.compare1$NEE, lower=lower$NEE, upper=upper$NEE)
head(NEE_dat)
NEE_dat=NEE_dat[!is.na(NEE_dat$NEE),]
head(NEE_dat)

NDVI_dat = data.frame(Time=data.compare1$time, NDVI=data.compare1$NDVI, lower=lower$NDVI, upper=upper$NDVI)
head(NDVI_dat)
NDVI_dat=NDVI_dat[!is.na(NDVI_dat$NDVI),]
head(NDVI_dat)
NDVI_dat = data.frame(Time=data.compare1$time, NDVI=data.compare1$NDVI, lower=lower$NDVI, upper=upper$NDVI)
head(NDVI_dat)
NDVI_dat=NDVI_dat[!is.na(NDVI_dat$NDVI),]
head(NDVI_dat)

#plot
par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(NEE~Time,data=NEE_dat,ylim=range(c(NEE_dat$lower,NEE_dat$upper)), type="p", pch=16, cex=0.5)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
arrows(NEE_dat$Time,NEE_dat$upper, NEE_dat$Time, NEE_dat$lower, angle=90, code=3, length=0.01, col="gray47")
points(NEE~Time, data=NEE_dat, pch=16, cex=0.75, col="blue")
abline(h=0)

plot(NDVI~Time,data=NDVI_dat,ylim=range(c(NDVI_dat$lower,NDVI_dat$upper)), type="p", pch=16, cex=0.5)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
arrows(NDVI_dat$Time,NDVI_dat$upper, NDVI_dat$Time, NDVI_dat$lower, angle=90, code=3, length=0.01, col="gray47")
points(NDVI~Time, data=NDVI_dat, pch=16, cex=0.75, col="blue")


#plot of NEE and NDVI together
par(mfrow=c(1,1), mar=c(2,2,2,2))
plot(NEE~DOY,data=data.assim[data.assim$Year==2009,],ylim=c(-3,1), xlim=c(1, 365), type="p", pch=16, cex=2, col="darkblue")
abline(h=0)
points(NDVI~DOY,data=data.assim[data.assim$Year==2009,],ylim=c(0,1), xlim=c(1, 365), type="p", pch=16, cex=2, col="gray30")


###########VALIDATION FIGURE##########
require(FME)

q05=apply(param.keep, 2, quantile, 0.05) #calculate 5% quantile
q25=apply(param.keep, 2, quantile, 0.25) #calculate 25% quantile
means=apply(param.keep, 2, mean)
q75=apply(param.keep, 2, quantile, 0.75) #calculate 75% quantile
q95=apply(param.keep, 2, quantile, 0.95) #calculate 95%
summarytable=data.frame(q05 = q05, q25 = q25, mean = means, 
                        q75 = q75, q95 = q95) #bind all of the information together in the proper order (same order as summarytable columns)


#global sensitivity analysis to find confidence intervals
sensvars = c("NEE",
             "Re",
             "GPP",
             "NDVI")


s.global <- sensRange(func=solvemodel, parms=means, sensvar = sensvars, parInput=param.keep)
s.global.summ = summary(s.global) #create summary table
head(s.global.summ) #view first 6 rows
tail(s.global.summ)

#get model output & confidence intervals organized
out=data.frame(solvemodel(param.best))
NEE_summ = data.frame(Time=s.global.summ[1:1461,1], NEE=out$NEE, sd=s.global.summ[1:1461,3], q05=s.global.summ[1:1461,6], q95=s.global.summ[1:1461,10], q25=s.global.summ[1:1461,7], q75=s.global.summ[1:1461,9])
head(NEE_summ)
#Re_summ = data.frame(Time=s.global.summ[1827:3652,1],Re=out$Re, sd=s.global.summ[1827:3652,3], q05=s.global.summ[1827:3652,6], q95=s.global.summ[1827:3652,10])
#head(Re_summ)
#GPP_summ = data.frame(Time=s.global.summ[3653:5478,1], GPP=out$GPP, sd=s.global.summ[3653:5478,3], q05=s.global.summ[3653:5478,6], q95=s.global.summ[3653:5478,10])
#head(GPP_summ)
NDVI_summ = data.frame(Time=s.global.summ[4384:5844,1], NDVI=out$NDVI, sd=s.global.summ[4384:5844,3], q05=s.global.summ[4384:5844,6], q95=s.global.summ[4384:5844,10], q25=s.global.summ[4384:5844,7], q75=s.global.summ[4384:5844,9])
head(NDVI_summ)
NDVI_summ=data.frame(cbind(NDVI_summ, low, high))
head(NDVI_summ)

#get data ready
data.compare2=read.csv("Assimilation_data_ALL.csv")
data.compare2=data.compare2[data.compare2$Year!=2013,]
data.compare_NEE=data.compare2[complete.cases(data.compare2[,6]),c(1:5,6)]
head(data.compare_NEE)
data.compare_Re=data.compare2[complete.cases(data.compare2[,7]),c(1:5,7)]
head(data.compare_Re)
data.compare_GPP=data.compare2[complete.cases(data.compare2[,8]),c(1:5,8)]
head(data.compare_GPP)
data.compare_NDVI=data.compare2[complete.cases(data.compare2[,9]),c(1:5,9)]
head(data.compare_NDVI)
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_Re = out[match(data.compare_Re$Time, out$time),]
out.compare_GPP = out[match(data.compare_GPP$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]


#preview a plot
plot(NEE~Time,data=NEE_summ,ylim=range(c(NEE_summ$q05,NEE_summ$q95)), type="p", pch=16, cex=0.5)
#make polygon where coordinates start with lower limit and then upper limit in reverse order
with(NEE_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = "grey75", border = FALSE))
points(NEE~Time, data=NEE_summ, pch=16, cex=0.5)
points(NEE~Time, data=data.compare_NEE, pch=16, col="blue", cex=0.5)



#linear regressions for all years
reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2011 | out.compare_NEE$year==2012]~data.compare_NEE$NEE[data.compare_NEE$Year==2011 | data.compare_NEE$Year==2012])
reg_Re = lm(out.compare_Re$Re~data.compare_Re$Re)
reg_GPP = lm(out.compare_GPP$GPP~data.compare_GPP$GPP)
reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2011 | out.compare_NDVI$year==2012]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011 | data.compare_NDVI$Year==2012])

#preview a plot
par(mfrow=c(1,1))
plot(out.compare_NEE$NEE~data.compare_NEE$NEE, pch=16, cex=0.75)
abline(0,1, col="red", lty=2, lwd=2)


#now need to calculate residuals and determine RMSE for each year

resid.GPP = data.compare_GPP$GPP - out.compare_GPP$GPP
resid_GPP = data.frame(time=out.compare_GPP$time, Year=out.compare_GPP$year, DOY=out.compare_GPP$DOY, resid.GPP)
resid.NEE = data.compare_NEE$NEE - out.compare_NEE$NEE
resid_NEE = data.frame(time=out.compare_NEE$time, Year=out.compare_NEE$year, DOY=out.compare_NEE$DOY, resid.NEE)
resid.Re = data.compare_Re$Re - out.compare_Re$Re
resid_Re = data.frame(time=out.compare_Re$time, Year=out.compare_Re$year, DOY=out.compare_Re$DOY, resid.Re)
head(resid_GPP)
head(resid_NEE)
head(resid_Re)

resid.NDVI = data.compare_NDVI$NDVI - out.compare_NDVI$NDVI
resid_NDVI = data.frame(time=out.compare_NDVI$time, Year=out.compare_NDVI$year, DOY=out.compare_NDVI$DOY, resid.NDVI)
head(resid_NDVI)


#calculate RMSE by year:
rmse <- function(x){
  sqrt(mean(x^2))
}
NEE_yrRMSE = tapply(resid_NEE$resid.NEE, resid_NEE$Year, rmse)
Re_yrRMSE = tapply(resid_Re$resid.Re, resid_Re$Year, rmse)
GPP_yrRMSE = tapply(resid_GPP$resid.GPP, resid_GPP$Year, rmse)
NDVI_yrRMSE = tapply(resid_NDVI$resid.NDVI, resid_NDVI$Year, rmse)
#preview a plot
barplot(NEE_yrRMSE)


#plot it together with barplots
par(mar=c(4,5,4,2))
#layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), 4, 3, byrow = TRUE), widths=c(3,1,1))
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE), widths=c(3,1,1))

plot(NEE~Time,data=NEE_summ,ylim=c(-4, 2), type="p", pch=16, cex=0.5, cex.axis=1.5, col="azure4")
#make polygon where coordinates start with lower limit and then upper limit in reverse order
with(NEE_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = "lightblue", border = FALSE))
abline(h=0)
points(NEE~Time, data=NEE_summ, pch=16, cex=0.75, col="gray40")
points(NEE~Time, data=data.compare_NEE, pch=16, col="darkblue", cex=0.75)
barplot(NEE_yrRMSE, ylim=c(0,max(NEE_yrRMSE)+0.5), cex.axis=1.5, col=c("gray50", "gray50", "gray80", "gray80"))
plot(out.compare_NEE$NEE[out.compare_NEE$year==2011 | out.compare_NEE$year==2012]~data.compare_NEE$NEE[data.compare_NEE$Year==2011 | data.compare_NEE$Year==2012], pch=16, xlab="", ylab="", cex=0.75, cex.axis=1.5, xlim=c(-4, 2), ylim=c(-4, 2))
#abline(reg_NEE, col="blue", lwd=2)
abline(0,1, col="red", lty=2, lwd=2)

plot(NDVI~Time,data=NDVI_summ,ylim=c(0,1), type="p", pch=16, cex=0.5, cex.axis=1.5, cex.lab=1.5, col="azure4")
#make polygon where coordinates start with lower limit and then upper limit in reverse order
with(NDVI_summ,polygon(c(Time,rev(Time)),c(q05,rev(q95)),col = "lightblue", border = FALSE))
points(NDVI~Time, data=NDVI_summ, pch=16, cex=0.75, col="gray40")
points(NDVI~Time, data=data.compare_NDVI, pch=16, col="darkblue", cex=0.75)
barplot(NDVI_yrRMSE, ylim=c(0,max(NDVI_yrRMSE)+0.02), cex.axis=1.5, cex.lab=1.5, col=c("gray50", "gray50", "gray80", "gray80"))
plot(out.compare_NDVI$NDVI[out.compare_NDVI$year==2011 | out.compare_NDVI$year==2012]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011 | data.compare_NDVI$Year==2012], xlab="", ylab="", pch=16, cex=0.75, cex.axis=1.5, xlim=c(0.3, 0.9), ylim=c(0.3, 0.9), cex.lab=1.5,)
#abline(reg_NDVI, col="blue", lwd=2)
abline(0,1, col="red", lty=2, lwd=2)




########MONTHLY VARIANCE DECOMPOSITION########
#first need to run the variance decomposition code in "8_OptimizationAnalysis.R"

par(mfrow=c(1,2), mar=c(5,5,3,2))

for (n in c(8,11)) { #for each output
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
  barplot(sub1, col=c("darkolivegreen3", "aquamarine", "darkgreen", "mediumseagreen",
                      "palegreen", "darkblue", "lightskyblue", "maroon4", "gray87", "azure2", 
                      "gray29", "gray57", "darkslategray"),             
          main=names(perc.all[n]), names.arg=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
          axisnames=TRUE, ylim=c(0,100), cex.axis=1.5, cex.names=1.5, ylab="% of Total Variance",
          xlab="Month", cex.lab=1.5, cex.main=2) #plot the data
} #end of for loop
legend("topright", legend=names(params),cex=1.5, ncol=1,
       fill=c("darkolivegreen3", "aquamarine", "darkgreen", "mediumseagreen",
              "palegreen", "darkblue", "lightskyblue", "maroon4", "gray87", "azure2", 
              "gray29", "gray57", "darkslategray"),)




########ANNUAL VARIANCE########
#need to run variance decomposition in "8_OptimizationAnalysis.R" first
par(mfrow=c(1,2), mar=c(2,2,2,2))
barplot(perc.all.NEE_all$NEE[1:8], names.arg=names(params[1:8]), cex.names=0.75, xlim=c(0,70),
        col=c("darkolivegreen3", "aquamarine", "darkgreen", "mediumseagreen",
              "palegreen", "darkblue", "lightskyblue", "maroon4"), horiz=TRUE, main="") #plot the data

barplot(perc.all.NDVI_all$NDVI[1:8], names.arg=names(params[1:8]), cex.names=0.75, xlim=c(0,70),
        col=c("darkolivegreen3", "aquamarine", "darkgreen", "mediumseagreen",
              "palegreen", "darkblue", "lightskyblue", "maroon4"), horiz=TRUE, main="") #plot the data


#get sensitivity values
s.local <- sensFun(func=solvemodel, parms=param.best, sensvar = sensvars) 
head(s.local); tail(s.local)
s.local.summ = data.frame(summary(s.local, var=T))
head(s.local.summ); tail(s.local.summ)
s.loc.summ.ordered = data.frame(s.local.summ[order(s.local.summ$var, abs(s.local.summ$Mean)),] )




######PLOT VARIANCE VS. UNCERTAINTY#########

#bind all together
varNDVI_all = (perc.all.NDVI_all$NDVI[2:5]/100)*perc.all.NDVI_all$NDVI[9]
varNEE_all = (perc.all.NEE_all$NEE[2:5]/100)*perc.all.NEE_all$NEE[9]
varNDVI_90 = (perc.all.NDVI_90$NDVI[2:5]/100)*perc.all.NDVI_90$NDVI[9]
varNEE_90 = (perc.all.NEE_90$NEE[2:5]/100)*perc.all.NEE_90$NEE[9]
varNDVI_70 = (perc.all.NDVI_70$NDVI[2:5]/100)*perc.all.NDVI_70$NDVI[9]
varNEE_70 = (perc.all.NEE_70$NEE[2:5]/100)*perc.all.NEE_70$NEE[9]
varNDVI_50 = (perc.all.NDVI_50$NDVI[2:5]/100)*perc.all.NDVI_50$NDVI[9]
varNEE_50 = (perc.all.NEE_50$NEE[2:5]/100)*perc.all.NEE_50$NEE[9]
varNDVI_20 = (perc.all.NDVI_20$NDVI[2:5]/100)*perc.all.NDVI_20$NDVI[9]
varNEE_20 = (perc.all.NEE_20$NEE[2:5]/100)*perc.all.NEE_20$NEE[9]

NDVI_var = c(varNDVI_all, varNDVI_90, varNDVI_70, varNDVI_50, varNDVI_20)
NEE_var = c(varNEE_all, varNEE_90, varNEE_70, varNEE_50, varNEE_20)
parameter = rep(rownames(perc.all.NEE_all)[2:5], 5)
range = rep(c(100,90,70,50,20), c(4,4,4,4,4))

var_all = data.frame(parameter=parameter, range=range, NEE=NEE_var, NDVI=NDVI_var)
head(var_all)

regNDVI_LitterRate = lm(log(NDVI)~range,data=var_all[var_all$parameter=="LitterRate",])
regNEE_LitterRate = lm(log(NEE)~range,data=var_all[var_all$parameter=="LitterRate",])

regNDVI_UptakeRate = lm(log(NDVI)~range, data=var_all[var_all$parameter=="UptakeRate",])
regNEE_UptakeRate = lm(log(NEE)~range, data=var_all[var_all$parameter=="UptakeRate",])

regNDVI_propN_fol = lm(log(NDVI)~range, data=var_all[var_all$parameter=="propN_fol",])
regNEE_propN_fol = lm(log(NEE)~range, data=var_all[var_all$parameter=="propN_fol",])

regNDVI_propN_roots = lm(log(NDVI)~range, data=var_all[var_all$parameter=="propN_roots",])
regNEE_propN_roots = lm(log(NEE)~range, data=var_all[var_all$parameter=="propN_roots",])

summary(regNDVI_LitterRate)
summary(regNEE_LitterRate)
summary(regNDVI_UptakeRate)
summary(regNEE_UptakeRate)
summary(regNDVI_propN_fol)
summary(regNEE_propN_fol)
summary(regNDVI_propN_roots)
summary(regNEE_propN_roots)


par(mfrow=c(1,2), mar=c(4,4,2,2))
plot(var_all$NEE[var_all$parameter=="LitterRate"]~var_all$range[var_all$parameter=="LitterRate"], pch=16, cex=1, col="aquamarine", ylim=c(min(var_all$NEE)-5, max(var_all$NEE)+5))
lines(var_all$range[var_all$parameter=="LitterRate"], exp(predict(regNEE_LitterRate,list(range=var_all$range[var_all$parameter=="LitterRate"]))), col="aquamarine", lwd=2)
points(var_all$NEE[var_all$parameter=="UptakeRate"]~var_all$range[var_all$parameter=="UptakeRate"], pch=16, cex=1, col="darkgreen")
lines(var_all$range[var_all$parameter=="UptakeRate"], exp(predict(regNEE_UptakeRate,list(range=var_all$range[var_all$parameter=="UptakeRate"]))), col="darkgreen", lwd=2)
points(var_all$NEE[var_all$parameter=="propN_fol"]~var_all$range[var_all$parameter=="propN_fol"], pch=16, cex=1, col="mediumseagreen")
lines(var_all$range[var_all$parameter=="propN_fol"], exp(predict(regNEE_propN_fol,list(range=var_all$range[var_all$parameter=="propN_fol"]))), col="mediumseagreen", lwd=2)
points(var_all$NEE[var_all$parameter=="propN_roots"]~var_all$range[var_all$parameter=="propN_roots"], pch=16, cex=1, col="palegreen")
lines(var_all$range[var_all$parameter=="propN_roots"], exp(predict(regNEE_propN_roots,list(range=var_all$range[var_all$parameter=="propN_roots"]))), col="palegreen", lwd=2)

plot(var_all$NDVI[var_all$parameter=="LitterRate"]~var_all$range[var_all$parameter=="LitterRate"], pch=16, cex=1, col="aquamarine", ylim=c(min(var_all$NDVI), max(var_all$NDVI)))
lines(var_all$range[var_all$parameter=="LitterRate"], exp(predict(regNDVI_LitterRate,list(range=var_all$range[var_all$parameter=="LitterRate"]))), col="aquamarine", lwd=2)
points(var_all$NDVI[var_all$parameter=="UptakeRate"]~var_all$range[var_all$parameter=="UptakeRate"], pch=16, cex=1, col="darkgreen")
lines(var_all$range[var_all$parameter=="UptakeRate"], exp(predict(regNDVI_UptakeRate,list(range=var_all$range[var_all$parameter=="UptakeRate"]))), col="darkgreen", lwd=2)
points(var_all$NDVI[var_all$parameter=="propN_fol"]~var_all$range[var_all$parameter=="propN_fol"], pch=16, cex=1, col="mediumseagreen")
lines(var_all$range[var_all$parameter=="propN_fol"], exp(predict(regNDVI_propN_fol,list(range=var_all$range[var_all$parameter=="propN_fol"]))), col="mediumseagreen", lwd=2)
points(var_all$NDVI[var_all$parameter=="propN_roots"]~var_all$range[var_all$parameter=="propN_roots"], pch=16, cex=1, col="palegreen")
lines(var_all$range[var_all$parameter=="propN_roots"], exp(predict(regNDVI_propN_roots,list(range=var_all$range[var_all$parameter=="propN_roots"]))), col="palegreen", lwd=2)


###########RELATIONSHIPS WITH FORCINGS#########


#CHECK RELATIONSHIPS WITH FORCINGS
par(mfrow=c(1,2), mar=c(4,4,2,2))
data.check = data[match(out.compare_NEE$time, data$time),]
plot(data.compare_NEE$NEE~data.check$PAR_ARF, pch=16, col="darkblue")
points(out.compare_NEE$NEE~data.check$PAR_ARF, col="gray40", pch=16)
abline(h=0)
plot(data.compare_NEE$NEE~data.check$Temp_ARF, pch=16, col="darkblue")
points(out.compare_NEE$NEE~data.check$Temp_ARF, col="gray40", pch=16)
abline(h=0)


