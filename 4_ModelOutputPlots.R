##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

out = data.frame(solvemodel(params)) #creates table of model output

#plot pools
par(mfrow=c(4,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="red", main = "Biomass C", xlab="", ylab="g C m-2")
#points(out1$Biomass_C~out1$time, col="gray1", cex=0.25)
plot(out$Biomass_N~out$time, type="l", col="red",  main = "Biomass N", xlab="", ylab="g N m-2", lty=2)
#points(out1$Biomass_N~out1$time, col="gray1", cex=0.25)
plot(out$SOM_C~out$time, type="l", col="red", main = "SOM C", xlab="", ylab="g C m-2")
#points(out1$SOM_C~out1$time, col="gray1", cex=0.25)
plot(out$SOM_N~out$time, type="l", col="red", main = "SOM N", xlab="Time (days)", ylab="g N m-2",lty=2)
#points(out1$SOM_N~out1$time, col="gray1", cex=0.25)
plot(out$Available_N~out$time, type="l", col="red", main = "Available N", xlab="Time (days)", ylab="g N m-2",lty=2)
#points(out1$Available_N~out1$time, col="gray1", cex=0.25)


#plot scalar
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(out$time, out$scaltemp, type="l")
plot(out$time, out$scal, type="l")

#see how well data matches
#to compare on 1:1 line with data, need to select only points for which data is available
data.compare2=read.csv("Assimilation_data_ALL.csv")
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

sigma.compare2=read.csv("Assimilation_sigma_ALL.csv")
sigma.compare_NEE=sigma.compare2[complete.cases(sigma.compare2[,6]),c(1:5,6)]
head(sigma.compare_NEE)
sigma.compare_Re=sigma.compare2[complete.cases(sigma.compare2[,7]),c(1:5,7)]
head(sigma.compare_Re)
sigma.compare_GPP=sigma.compare2[complete.cases(sigma.compare2[,8]),c(1:5,8)]
head(sigma.compare_GPP)
sigma.compare_NDVI=sigma.compare2[complete.cases(sigma.compare2[,9]),c(1:5,9)]
head(sigma.compare_NDVI)

#PLOT DATA FOR ALL YEARS
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE~out$time, col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE, out.compare_NEE$NEE, ylim=c(-4, 1), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re~out$time, col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re, out.compare_Re$Re, ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP~out$time, col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP, out.compare_GPP$GPP, ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI~out$time, col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI, out.compare_NDVI$NDVI, ylab="Model", xlab="Data")
abline(0,1, col="red")


#PLOT DATA FOR ONE YEAR
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2013], out.compare_NEE$NEE[out.compare_NEE$year==2013], ylim=c(-4, 1), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re[out$year==2013]~out$time[out$year==2013], col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re[data.compare_Re$Year==2013], out.compare_Re$Re[out.compare_Re$year==2013], ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2013], out.compare_GPP$GPP[out.compare_GPP$year==2013], ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013], out.compare_NDVI$NDVI[out.compare_NDVI$year==2013], ylab="Model", xlab="Data")
abline(0,1, col="red")



#linear regressions
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data.compare_NEE$NEE, out.compare_NEE$NEE, xlab= "Actual", ylab="Modelled", main = "NEE")
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



##NEED TO KEEP WORKING ON THIS##
resid.GPP = data.compare_GPP$GPP - out.compare_GPP$GPP
resid_GPP = data.frame(time=out.compare_GPP$time, DOY=out.compare_GPP$DOY, resid.GPP)
resid.NEE = data.compare_NEE$NEE - out.compare_NEE$NEE
resid_NEE = data.frame(time=out.compare_NEE$time, DOY=out.compare_NEE$DOY, resid.NEE)
resid.Re = data.compare_Re$Re - out.compare_Re$Re
resid_Re = data.frame(time=out.compare_Re$time, DOY=out.compare_Re$DOY, resid.Re)
head(resid_GPP)
head(resid_NEE)
head(resid_Re)

resid.NDVI = data.compare_NDVI$NDVI - out.compare_NDVI$NDVI
resid_NDVI = data.frame(time=out.compare_NDVI$time, DOY=out.compare_NDVI$DOY, resid.NDVI)
head(resid_NDVI)

par(mfrow=c(4,1), mar=c(4,4,2,2))
plot(resid_NEE$DOY, resid_NEE$resid.NEE, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_Re$DOY, resid_Re$resid.Re, main="Re Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_GPP$DOY, resid_GPP$resid.GPP, main="GPP Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_NDVI$DOY, resid_NDVI$resid.NDVI, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")

rmse <- function(x){
  sqrt(mean(x^2))
}
rmse.GPP=rmse(resid.GPP)
rmse.NEE=rmse(resid.NEE)
rmse.Re=rmse(resid.Re)
rmse.NDVI=rmse(resid.NDVI)
rmse.GPP;rmse.NEE;rmse.Re;rmse.NDVI

resid.GPP.mean = tapply(resid_GPP$resid.GPP, resid_GPP$DOY, mean, na.rm=TRUE)
resid.NEE.mean = tapply(resid_NEE$resid.NEE, resid_NEE$DOY, mean, na.rm=TRUE)
resid.Re.mean = tapply(resid_Re$resid.Re, resid_Re$DOY, mean, na.rm=TRUE)
resid.NDVI.mean = tapply(resid_NDVI$resid.NDVI, resid_NDVI$DOY, mean, na.rm=TRUE)
time=seq(140,250,1)
resid.meansflux=data.frame(DOY=time, resid.GPP.mean, resid.NEE.mean, resid.Re.mean)
time=seq(144,250,1)
resid.meansNDVI=data.frame(DOY=time, resid.NDVI.mean)

par(mfrow=c(4,1), mar=c(4,4,2,2))
plot(resid.meansflux$DOY, resid.meansflux$resid.NEE.mean, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansflux$DOY, resid.meansflux$resid.Re.mean, main="Re Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansflux$DOY, resid.meansflux$resid.GPP.mean, main="GPP Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansNDVI$DOY, resid.meansNDVI$resid.NDVI.mean, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")


rmse.GPP=rmse(resid.GPP.mean)
rmse.NEE=rmse(resid.NEE.mean)
rmse.Re=rmse(resid.Re.mean)
rmse.NDVI=rmse(resid.NDVI.mean)
rmse.GPP;rmse.NEE;rmse.Re;rmse.NDVI

mae.GPP=mean(abs(resid.GPP.mean))
mae.NEE=mean(abs(resid.NEE.mean))
mae.Re=mean(abs(resid.Re.mean))
mae.NDVI=mean(abs(resid.NDVI.mean))
mae.GPP;mae.NEE;mae.Re;mae.NDVI





#PLOT BEFORE AND AFTER SCALAR
head(data.compare2)
head(data.compare_NEE)
head(data.compare_GPP)
head(data.compare_Re)
head(data.compare_NDVI)
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_Re = out[match(data.compare_Re$Time, out$time),]
out.compare_GPP = out[match(data.compare_GPP$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]
out.compare_NEE_ns = out_ns[match(data.compare_NEE$Time, out_ns$time),]
out.compare_Re_ns = out_ns[match(data.compare_Re$Time, out_ns$time),]
out.compare_GPP_ns = out_ns[match(data.compare_GPP$Time, out_ns$time),]
out.compare_NDVI_ns = out_ns[match(data.compare_NDVI$Time, out_ns$time),]

par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(out.compare_GPP_ns$GPP~out.compare_GPP_ns$DOY, xlim=c(1,365), ylim=c(0,8), pch=16)
points(data.compare_GPP$GPP~data.compare_GPP$DOY, col="red")
plot(out.compare_GPP$GPP~out.compare_GPP$DOY, xlim=c(1,365), ylim=c(0,8), pch=16)
points(data.compare_GPP$GPP~data.compare_GPP$DOY, col="red")
plot(out.compare_NDVI_ns$NDVI~out.compare_NDVI_ns$DOY, xlim=c(1,365), ylim=c(0,1), pch=16)
points(data.compare_NDVI$NDVI~data.compare_NDVI$DOY, col="red")
plot(out.compare_NDVI$NDVI~out.compare_NDVI$DOY, xlim=c(1,365), ylim=c(0,1), pch=16)
points(data.compare_NDVI$NDVI~data.compare_NDVI$DOY, col="red")

#plot data
par(mfrow=c(1,1), mar=c(4,4,1,4))
plot(data.compare_GPP$Time, data.compare_GPP$GPP, ylim=c(0, 8), xlab="Time", ylab="") # first plot
par(new = TRUE)
plot(data.compare_NDVI$Time, data.compare_NDVI$NDVI, ylim=c(0, 0.8), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,0.7), col="red",col.axis="red",las=1)

#plot without scalar
plot(out.compare_GPP_ns$time, out.compare_GPP_ns$GPP, xlim=c(1,1318), ylim=c(0, 8), xlab="Time", ylab="") # first plot
par(new = TRUE)
plot(out.compare_NDVI_ns$time, out.compare_NDVI_ns$NDVI, xlim=c(1,1318), ylim=c(0, 0.8), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,0.7), col="red",col.axis="red",las=1)

#plot with scalar
plot(out.compare_GPP$time, out.compare_GPP$GPP, xlim=c(1,1318), ylim=c(0, 8), xlab="Time", ylab="") # first plot
par(new = TRUE)
plot(out.compare_NDVI$time, out.compare_NDVI$NDVI, xlim=c(1,1318), ylim=c(0, 0.8), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,0.7), col="red",col.axis="red",las=1)



#CHECK RELATIONSHIPS WITH FORCINGS
data.check = data[match(out.compare_NEE$time, data$time),]
plot(data.compare_NEE$NEE~data.check$PAR_ARF, pch=16)
points(out.compare_NEE$NEE~data.check$PAR_ARF, col="red")

