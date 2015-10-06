##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

out = data.frame(solvemodel(param.best, state)) #creates table of model output

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
plot(out$time, out$scalGDD, type="l")
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


par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE~out$time, col="azure4", pch=18, ylim=c(-10,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
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

plot(out$NDVI~out$time, col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", type="l")
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI, out.compare_NDVI$NDVI, ylab="Model", xlab="Data")
abline(0,1, col="red")


##NEED TO KEEP WORKING ON THIS##
resid.GPP = data.compare_GPP$GPP - out.compare_GPP$GPP
resid_flux = data.frame(time=out.compare_flux$time, DOY=out.compare_flux$DOY, resid.GPP, resid.NEE, resid.Re)
resid.NEE = data.compare_NEE$NEE - out.compare_NEE$NEE
resid_flux = data.frame(time=out.compare_flux$time, DOY=out.compare_flux$DOY, resid.GPP, resid.NEE, resid.Re)
resid.Re = data.compare_Re$Re - out.compare_Re$Re
resid_flux = data.frame(time=out.compare_flux$time, DOY=out.compare_flux$DOY, resid.GPP, resid.NEE, resid.Re)


resid.NDVI = data.compare_NDVI$NDVI - out.compare_NDVI$NDVI
resid_NDVI = data.frame(time=out.compare_NDVI$time, DOY=out.compare_NDVI$DOY, resid.NDVI)
head(resid_NDVI)

par(mfrow=c(4,1), mar=c(4,4,2,2))
plot(resid_flux$DOY, resid_flux$resid.GPP, main="GPP Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_flux$DOY, resid_flux$resid.Re, main="Re Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_flux$DOY, resid_flux$resid.NEE, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid_NDVI$DOY, resid_NDVI$resid.NDVI, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")


rmse.GPP=sqrt(mean((resid.GPP)^2))
rmse.NEE=sqrt(mean((resid.NEE)^2))
rmse.Re=sqrt(mean((resid.Re)^2))
rmse.NDVI=sqrt(mean((resid.NDVI)^2))
rmse.GPP;rmse.NEE;rmse.Re;rmse.NDVI

resid.GPP.mean = tapply(resid_flux$resid.GPP, resid_flux$DOY, mean)
resid.NEE.mean = tapply(resid_flux$resid.NEE, resid_flux$DOY, mean)
resid.Re.mean = tapply(resid_flux$resid.Re, resid_flux$DOY, mean)
resid.NDVI.mean = tapply(resid_NDVI$resid.NDVI, resid_NDVI$DOY, mean)
resid.DOYflux=unique(resid_flux$DOY)
resid.DOYNDVI=unique(resid_NDVI$DOY)
resid.meansflux=data.frame(DOY=resid.DOYflux, resid.GPP.mean, resid.NEE.mean, resid.Re.mean)
resid.meansNDVI=data.frame(DOY=resid.DOYNDVI, resid.NDVI.mean)

par(mfrow=c(4,1), mar=c(4,4,2,2))
plot(resid.meansflux$DOY, resid.meansflux$resid.GPP.mean, main="GPP Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansflux$DOY, resid.meansflux$resid.Re.mean, main="Re Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansflux$DOY, resid.meansflux$resid.NEE.mean, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.meansNDVI$DOY, resid.meansNDVI$resid.NDVI.mean, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")


rmse.GPP=sqrt(mean((resid.GPP.mean)^2))
rmse.NEE=sqrt(mean((resid.NEE.mean)^2))
rmse.Re=sqrt(mean((resid.Re.mean)^2))
rmse.NDVI=sqrt(mean((resid.NDVI.mean)^2))
rmse.GPP;rmse.NEE;rmse.Re;rmse.NDVI

mae.GPP=mean(abs(resid.GPP.mean))
mae.NEE=mean(abs(resid.NEE.mean))
mae.Re=mean(abs(resid.Re.mean))
mae.NDVI=mean(abs(resid.NDVI.mean))
mae.GPP;mae.NEE;mae.Re;mae.NDVI






#PLOT BEFORE AND AFTER SCALAR
data.compare2=read.csv("Assimilation_data_ALL.csv")
data.compare_flux=data.compare2[complete.cases(data.compare2[,6]),c(1:7,9)]
head(data.compare_flux)
data.compare_NDVI=data.compare2[complete.cases(data.compare2[,8]),c(1:5,8)]
out.compare_flux = out[match(data.compare_flux$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]
outns.compare_flux = out_ns[match(data.compare_flux$Time, out_ns$time),]
outns.compare_NDVI = out_ns[match(data.compare_NDVI$Time, out_ns$time),]


par(mfrow=c(2,2), mar=c(4,4,1,1))
plot(outns.compare_flux$GPP~outns.compare_flux$DOY, xlim=c(1,365), ylim=c(0,8))
points(data.compare_flux$GPP~data.compare_flux$DOY, col="red", pch=16)
plot(out.compare_flux$GPP~out.compare_flux$DOY, ylim=c(0,8))
points(data.compare_flux$GPP~data.compare_flux$DOY, col="red", pch=16)
plot(outns.compare_NDVI$NDVI~outns.compare_NDVI$DOY, xlim=c(1,365), ylim=c(0,1))
points(data.compare_NDVI$NDVI~data.compare_NDVI$DOY, col="red", pch=16)
plot(out.compare_NDVI$NDVI~out.compare_NDVI$DOY, ylim=c(0,1))
points(data.compare_NDVI$NDVI~data.compare_NDVI$DOY, col="red", pch=16)

#plot data
par(mfrow=c(1,1), mar=c(4,4,1,4))
plot(data.compare_flux$Time, data.compare_flux$GPP, xlim=c(1,1318), ylim=c(0, 8), xlab="Time", ylab="") # first plot
par(new = TRUE)
plot(data.compare_NDVI$Time, data.compare_NDVI$NDVI, xlim=c(1,1318), ylim=c(0, 0.8), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,0.7), col="red",col.axis="red",las=1)

#plot without scalar
plot(outns.compare_flux$time, outns.compare_flux$GPP, xlim=c(1,1318), ylim=c(0, 8), xlab="Time", ylab="") # first plot
par(new = TRUE)
plot(outns.compare_NDVI$time, outns.compare_NDVI$NDVI, xlim=c(1,1318), ylim=c(0, 0.8), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,0.7), col="red",col.axis="red",las=1)

#plot with scalar
plot(out.compare_flux$time, out.compare_flux$GPP, xlim=c(1,1318), ylim=c(0, 8), xlab="Time", ylab="") # first plot
par(new = TRUE)
plot(out.compare_NDVI$time, out.compare_NDVI$NDVI, xlim=c(1,1318), ylim=c(0, 0.8), pch=16, col="red", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(4, ylim=c(0,0.7), col="red",col.axis="red",las=1)



