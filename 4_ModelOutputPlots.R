##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")



#plot pools
par(mfrow=c(4,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="springgreen3", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="springgreen3",  main = "Biomass N", xlab="", ylab="g N m-2", lty=2)
plot(out$Litter_C~out$time, type="l", col="orange", main = "Litter C", xlab="", ylab="g C m-2")
plot(out$Litter_N~out$time, type="l", col="orange", main = "Litter N", xlab="", ylab="g N m-2", lty=2)
plot(out$SOM_C~out$time, type="l", col="red", main = "SOM C", xlab="", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="red", main = "SOM N", xlab="Time (days)", ylab="g N m-2",lty=2)
plot(out$Available_N~out$time, type="l", col="green", main = "Available N", xlab="Time (days)", ylab="g N m-2",lty=2)

#plot scalar
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(out$time, out$scalGDD, type="l")
plot(out$time, out$scalTEMP, type="l")
plot(out$time, out$scal, type="l")

#see how well data matches
#to compare on 1:1 line with data, need to select only points for which data is available
data.compare=read.csv("ALLData_Assim.csv")
data.compare=data.compare[complete.cases(data.compare),]
head(data.compare)
out.compare = out[match(data.compare$time, out$time),]
head(out.compare)

par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$GPP~out$time, col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data$GPP, col="blue", pch=18, cex=0.8)
plot(data.compare$GPP, out.compare$GPP, ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re~out$time, col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data$Re, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare$Re, out.compare$Re, ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NEE~out$time, col="azure4", pch=18, ylim=c(-5,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data$NEE, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare$NEE, out.compare$NEE, ylim=c(-4, 1), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI~out$time, col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", type="l", ylim=c(0, 1))
points(data$NDVI, col="blue", pch=18, cex=0.8)
plot(data.compare$NDVI, out.compare$NDVI, ylab="Model", xlab="Data")
abline(0,1, col="red")


resid.GPP = data.compare$GPP - out.compare$GPP
resid.NEE = data.compare$NEE - out.compare$NEE
resid.Re = data.compare$Re - out.compare$Re
resid.NDVI = data.compare$NDVI - out.compare$NDVI
resid = data.frame(time=out.compare$time, DOY=out.compare$DOY, resid.GPP, resid.NEE, resid.Re, resid.NDVI)
head(resid)

par(mfrow=c(4,1), mar=c(4,4,2,2))
plot(resid$DOY, resid$resid.GPP, main="GPP Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid$DOY, resid$resid.Re, main="Re Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid$DOY, resid$resid.NEE, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid$DOY, resid$resid.NDVI, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")


rmse.GPP=sqrt(mean((resid.GPP)^2))
rmse.NEE=sqrt(mean((resid.NEE)^2))
rmse.Re=sqrt(mean((resid.Re)^2))
rmse.NDVI=sqrt(mean((resid.NDVI)^2))
rmse.GPP;rmse.NEE;rmse.Re;rmse.NDVI

resid.GPP.mean = tapply(resid$resid.GPP, resid$DOY, mean)
resid.NEE.mean = tapply(resid$resid.NEE, resid$DOY, mean)
resid.Re.mean = tapply(resid$resid.Re, resid$DOY, mean)
resid.NDVI.mean = tapply(resid$resid.NDVI, resid$DOY, mean)
resid.DOY=unique(resid$DOY)
resid.means=data.frame(DOY=resid.DOY, resid.GPP.mean, resid.NEE.mean, resid.Re.mean, resid.NDVI.mean)

par(mfrow=c(4,1), mar=c(4,4,2,2))
plot(resid.means$DOY, resid.means$resid.GPP.mean, main="GPP Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.means$DOY, resid.means$resid.Re.mean, main="Re Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.means$DOY, resid.means$resid.NEE.mean, main="NEE Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")
plot(resid.means$DOY, resid.means$resid.NDVI.mean, main="NDVI Residuals", ylab="Residuals", xlab="DOY")
abline(h=0, col="red")


rmse.GPP=sqrt(mean((resid.GPP.mean)^2))
rmse.NEE=sqrt(mean((resid.NEE.mean)^2))
rmse.Re=sqrt(mean((resid.Re.mean)^2))
rmse.NDVI=sqrt(mean((resid.NDVI.mean)^2))
rmse.GPP;rmse.NEE;rmse.Re;rmse.NDVI


par(mfrow=c(3,1), mar=c(4,4,2,2))
plot(data$GPP~data$PAR_vis, pch=16, ylab="GPP", xlab="PAR_vis")
points(out$GPP~data$PAR_vis, col="red")

plot(data$Re~data$Temp_ARF, pch=16, ylab="Re", xlab="Temperature")
points(out$Re~data$Temp_ARF, col="red")

plot(data$NEE~data$Temp_ARF, pch=16, ylab="NEE", xlab="Temperature")
points(out$NEE~data$Temp_ARF, col="red")

