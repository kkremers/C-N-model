##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")

out = data.frame(solvemodel(params)) #creates table of model output

#plot pools
par(mfrow=c(4,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="forestgreen", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="forestgreen",  main = "Biomass N", xlab="", ylab="g N m-2")
plot(out$SOM_C~out$time, type="l", col="brown", main = "SOM C", xlab="", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="brown", main = "SOM N", xlab="Time (days)", ylab="g N m-2")
plot(out$Available_N~out$time, type="l", col="blue", main = "Available_ N", xlab="Time (days)", ylab="g N m-2",lty=2)

#plot scalar
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF)
lines(Temp.sm, col="red", lwd=3)
plot(scal.temp.sm)

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

#PLOT DATA FOR ALL YEARS
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE~out$time, col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE, out.compare_NEE$NEE, ylim=c(-4, 4), xlim=c(-4,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re~out$time, col="azure4", pch=18, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re, out.compare_Re$Re, ylim=c(-1, 4), xlim=c(-1,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP~out$time, col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP, out.compare_GPP$GPP, ylim=c(-1, 10), xlim=c(-1,8), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI~out$time, col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0,1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_NDVI$NDVI, out.compare_NDVI$NDVI, ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1) )
abline(0,1, col="red")


#PLOT DATA FOR 2009
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2009]~out$time[out$year==2009], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2009], out.compare_NEE$NEE[out.compare_NEE$year==2009],ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re[out$year==2009]~out$time[out$year==2009], col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re[data.compare_Re$Year==2009], out.compare_Re$Re[out.compare_Re$year==2009], ylim=c(-1, 4), xlim=c(-1,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP[out$year==2009]~out$time[out$year==2009], col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2009], out.compare_GPP$GPP[out.compare_GPP$year==2009], ylim=c(-1, 8), xlim=c(-1,8), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2009]~out$time[out$year==2009], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2009], out.compare_NDVI$NDVI[out.compare_NDVI$year==2009], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2010
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2010]~out$time[out$year==2010], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2010], out.compare_NEE$NEE[out.compare_NEE$year==2010], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re[out$year==2010]~out$time[out$year==2010], col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re[data.compare_Re$Year==2010], out.compare_Re$Re[out.compare_Re$year==2010], ylim=c(-1, 4), xlim=c(-1,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP[out$year==2010]~out$time[out$year==2010], col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2010], out.compare_GPP$GPP[out.compare_GPP$year==2010], ylim=c(-1, 8), xlim=c(-1,8), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2010]~out$time[out$year==2010], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2010], out.compare_NDVI$NDVI[out.compare_NDVI$year==2010], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2011
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2011]~out$time[out$year==2011], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2011], out.compare_NEE$NEE[out.compare_NEE$year==2011], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re[out$year==2011]~out$time[out$year==2011], col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re[data.compare_Re$Year==2011], out.compare_Re$Re[out.compare_Re$year==2011], ylim=c(-1, 4), xlim=c(-1,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP[out$year==2011]~out$time[out$year==2011], col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2011], out.compare_GPP$GPP[out.compare_GPP$year==2011], ylim=c(-1, 8), xlim=c(-1,8), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2011]~out$time[out$year==2011], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011], out.compare_NDVI$NDVI[out.compare_NDVI$year==2011], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2012
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2012]~out$time[out$year==2012], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2012], out.compare_NEE$NEE[out.compare_NEE$year==2012], ylim=c(-4, 4), xlim=c(-4,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re[out$year==2012]~out$time[out$year==2012], col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re[data.compare_Re$Year==2012], out.compare_Re$Re[out.compare_Re$year==2012], ylim=c(-1, 4), xlim=c(-1,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP[out$year==2012]~out$time[out$year==2012], col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2012], out.compare_GPP$GPP[out.compare_GPP$year==2012], ylim=c(-1, 8), xlim=c(-1,8), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2012]~out$time[out$year==2012], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2012], out.compare_NDVI$NDVI[out.compare_NDVI$year==2012], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")

#PLOT DATA FOR 2013
par(mfrow=c(4,2), mar=c(4,4,2,2))
plot(out$NEE[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylim=c(-6,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data.compare2$NEE~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2013], out.compare_NEE$NEE[out.compare_NEE$year==2013], ylim=c(-4, 4), xlim=c(-4,4),  ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(-out$Re[out$year==2013]~out$time[out$year==2013], col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data.compare2$Re~data.compare2$Time, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare_Re$Re[data.compare_Re$Year==2013], out.compare_Re$Re[out.compare_Re$year==2013], ylim=c(-1, 4), xlim=c(-1,4), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$GPP[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="Time (days)", type="l")
points(data.compare2$GPP~data.compare2$Time, col="blue", pch=18, cex=0.8)
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2013], out.compare_GPP$GPP[out.compare_GPP$year==2013], ylim=c(-1, 8), xlim=c(-1,8), ylab="Model", xlab="Data")
abline(0,1, col="red")

plot(out$NDVI[out$year==2013]~out$time[out$year==2013], col="azure4", pch=18, ylab="NDVI", xlab="Time(days)", ylim=c(0, 1))
points((data.compare_NDVI$NDVI)~data.compare_NDVI$Time, col="blue", pch=18, cex=0.8)
#arrows(data.compare_NDVI$Time, data.compare_NDVI$NDVI-sigma.compare_NDVI$NDVI, data.compare_NDVI$Time, data.compare_NDVI$NDVI+sigma.compare_NDVI$NDVI, length=0.05, angle=90, code=3)
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013], out.compare_NDVI$NDVI[out.compare_NDVI$year==2013], ylab="Model", xlab="Data", ylim=c(0, 1), xlim=c(0,1))
abline(0,1, col="red")



#linear regressions for all years
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_Re = out[match(data.compare_Re$Time, out$time),]
out.compare_GPP = out[match(data.compare_GPP$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE~data.compare_NEE$NEE)
plot(data.compare_NEE$NEE, out.compare_NEE$NEE, xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re~data.compare_Re$Re)
plot(data.compare_Re$Re, out.compare_Re$Re, xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re,col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP~data.compare_GPP$GPP)
plot(data.compare_GPP$GPP, out.compare_GPP$GPP, xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP,col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI~data.compare_NDVI$NDVI)
plot(data.compare_NDVI$NDVI, out.compare_NDVI$NDVI, xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI,col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")


summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)


#linear regressions for 2009
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2009]~data.compare_NEE$NEE[data.compare_NEE$Year==2009])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2009], out.compare_NEE$NEE[out.compare_NEE$year==2009], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re[out.compare_Re$year==2009]~data.compare_Re$Re[data.compare_Re$Year==2009])
plot(data.compare_Re$Re[data.compare_Re$Year==2009], out.compare_Re$Re[out.compare_Re$year==2009], xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re, col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP[out.compare_GPP$year==2009]~data.compare_GPP$GPP[data.compare_GPP$Year==2009])
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2009], out.compare_GPP$GPP[out.compare_GPP$year==2009], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP, col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2009]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2009])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2009], out.compare_NDVI$NDVI[out.compare_NDVI$year==2009], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)



#linear regressions for 2010
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2010]~data.compare_NEE$NEE[data.compare_NEE$Year==2010])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2010], out.compare_NEE$NEE[out.compare_NEE$year==2010], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re[out.compare_Re$year==2010]~data.compare_Re$Re[data.compare_Re$Year==2010])
plot(data.compare_Re$Re[data.compare_Re$Year==2010], out.compare_Re$Re[out.compare_Re$year==2010], xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re, col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP[out.compare_GPP$year==2010]~data.compare_GPP$GPP[data.compare_GPP$Year==2010])
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2010], out.compare_GPP$GPP[out.compare_GPP$year==2010], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP, col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2010]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2010])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2010], out.compare_NDVI$NDVI[out.compare_NDVI$year==2010], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)



#linear regressions for 2011
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2011]~data.compare_NEE$NEE[data.compare_NEE$Year==2011])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2011], out.compare_NEE$NEE[out.compare_NEE$year==2011], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re[out.compare_Re$year==2011]~data.compare_Re$Re[data.compare_Re$Year==2011])
plot(data.compare_Re$Re[data.compare_Re$Year==2011], out.compare_Re$Re[out.compare_Re$year==2011], xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re, col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP[out.compare_GPP$year==2011]~data.compare_GPP$GPP[data.compare_GPP$Year==2011])
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2011], out.compare_GPP$GPP[out.compare_GPP$year==2011], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP, col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2011]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2011], out.compare_NDVI$NDVI[out.compare_NDVI$year==2011], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)



#linear regressions for 2012
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2012]~data.compare_NEE$NEE[data.compare_NEE$Year==2012])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2012], out.compare_NEE$NEE[out.compare_NEE$year==2012], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re[out.compare_Re$year==2012]~data.compare_Re$Re[data.compare_Re$Year==2012])
plot(data.compare_Re$Re[data.compare_Re$Year==2012], out.compare_Re$Re[out.compare_Re$year==2012], xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re, col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP[out.compare_GPP$year==2012]~data.compare_GPP$GPP[data.compare_GPP$Year==2012])
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2012], out.compare_GPP$GPP[out.compare_GPP$year==2012], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP, col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2012]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2012])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2012], out.compare_NDVI$NDVI[out.compare_NDVI$year==2012], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)


#linear regressions for 2013
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year==2013]~data.compare_NEE$NEE[data.compare_NEE$Year==2013])
plot(data.compare_NEE$NEE[data.compare_NEE$Year==2013], out.compare_NEE$NEE[out.compare_NEE$year==2013], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re[out.compare_Re$year==2013]~data.compare_Re$Re[data.compare_Re$Year==2013])
plot(data.compare_Re$Re[data.compare_Re$Year==2013], out.compare_Re$Re[out.compare_Re$year==2013], xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re, col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP[out.compare_GPP$year==2013]~data.compare_GPP$GPP[data.compare_GPP$Year==2013])
plot(data.compare_GPP$GPP[data.compare_GPP$Year==2013], out.compare_GPP$GPP[out.compare_GPP$year==2013], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP, col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year==2013]~data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year==2013], out.compare_NDVI$NDVI[out.compare_NDVI$year==2013], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)

#linear regressions for 2009-2012
par(mfrow=c(4,2), mar=c(4,4,2,2))

reg_NEE = lm(out.compare_NEE$NEE[out.compare_NEE$year!=2013]~data.compare_NEE$NEE[data.compare_NEE$Year!=2013])
plot(data.compare_NEE$NEE[data.compare_NEE$Year!=2013], out.compare_NEE$NEE[out.compare_NEE$year!=2013], xlab= "Actual", ylab="Modelled", main = "NEE")
abline(reg_NEE, col="red")
plot(density(resid(reg_NEE)), main="Density of Residuals")

reg_Re = lm(out.compare_Re$Re[out.compare_Re$year!=2013]~data.compare_Re$Re[data.compare_Re$Year!=2013])
plot(data.compare_Re$Re[data.compare_Re$Year!=2013], out.compare_Re$Re[out.compare_Re$year!=2013], xlab= "Actual", ylab="Modelled", main = "Re")
abline(reg_Re, col="red")
plot(density(resid(reg_Re)), main="Density of Residuals")

reg_GPP = lm(out.compare_GPP$GPP[out.compare_GPP$year!=2013]~data.compare_GPP$GPP[data.compare_GPP$Year!=2013])
plot(data.compare_GPP$GPP[data.compare_GPP$Year!=2013], out.compare_GPP$GPP[out.compare_GPP$year!=2013], xlab= "Actual", ylab="Modelled", main = "GPP")
abline(reg_GPP, col="red")
plot(density(resid(reg_GPP)), main="Density of Residuals")

reg_NDVI = lm(out.compare_NDVI$NDVI[out.compare_NDVI$year!=2013]~data.compare_NDVI$NDVI[data.compare_NDVI$Year!=2013])
plot(data.compare_NDVI$NDVI[data.compare_NDVI$Year!=2013], out.compare_NDVI$NDVI[out.compare_NDVI$year!=2013], xlab= "Actual", ylab="Modelled", main = "NDVI")
abline(reg_NDVI, col="red")
plot(density(resid(reg_NDVI)), main="Density of Residuals")

summary(reg_NEE)
summary(reg_Re)
summary(reg_GPP)
summary(reg_NDVI)



####RESIDUAL ANALYSIS###
out.compare_NEE = out[match(data.compare_NEE$Time, out$time),]
out.compare_Re = out[match(data.compare_Re$Time, out$time),]
out.compare_GPP = out[match(data.compare_GPP$Time, out$time),]
out.compare_NDVI = out[match(data.compare_NDVI$Time, out$time),]

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

rmse.NEE=rmse(resid.NEE)
rmse.Re=rmse(resid.Re)
rmse.GPP=rmse(resid.GPP)
rmse.NDVI=rmse(resid.NDVI)
rmse.NEE;rmse.Re;rmse.GPP;rmse.NDVI

#mean residuals across years
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



rmse.NEE=rmse(resid.NEE.mean)
rmse.Re=rmse(resid.Re.mean)
rmse.GPP=rmse(resid.GPP.mean)
rmse.NDVI=rmse(resid.NDVI.mean)
rmse.NEE;rmse.Re;rmse.GPP;rmse.NDVI

#calculate RMSE by year:

tapply(resid_NEE$resid.NEE, resid_NEE$Year, rmse)
tapply(resid_Re$resid.Re, resid_Re$Year, rmse)
tapply(resid_GPP$resid.GPP, resid_GPP$Year, rmse)
tapply(resid_NDVI$resid.NDVI, resid_NDVI$Year, rmse)

#calculate RMSE with 2013 removed

rmse.NEE=rmse(resid_NEE$resid.NEE[resid_NEE$Year!=2013])
rmse.Re=rmse(resid_Re$resid.Re[resid_Re$Year!=2013])
rmse.GPP=rmse(resid_GPP$resid.GPP[resid_GPP$Year!=2013])
rmse.NDVI=rmse(resid_NDVI$resid.NDVI[resid_NDVI$Year!=2013])
rmse.NEE;rmse.Re;rmse.GPP;rmse.NDVI



##look at relationship between spring NEE RMSE and DOY.minGDD
#first, filter data for spring only
head(resid_NEE)
rmse.NEE = tapply(resid_NEE$resid.NEE, resid_NEE$Year, rmse)
RMSE.spring=NULL
years = unique(data$year) #tells you which years we have data for 
for (i in 1: length(years)){
  year.i = years[i] #select year
  data.year = subset(resid_NEE, resid_NEE$Year==year.i) #subset for that year
  resid_NEE.spring = data.year[data.year$DOY<=mid.day[i],] #subset for spring
  RMSE.spring[i] = rmse(resid_NEE.spring$resid.NEE) #calculate rmse
}
RMSE.spring #show yearly values
melt.day #show day of melt
frost.day #show day of frost
length.season = frost.day-melt.day
length.season
NEE_springRMSE = data.frame(RMSEspring = RMSE.spring, RMSEall = rmse.NEE, melt.day = melt.day, frost.day=frost.day, length=length.season)#create data frame
NEE_springRMSE #view

par(mfrow=c(3,1))
reg.melt = lm(NEE_springRMSE$RMSEall~NEE_springRMSE$melt.day) #run linear model
plot(NEE_springRMSE$RMSEall~NEE_springRMSE$melt.day) #plot data
abline(reg.melt, col="red") #add model line
summary(reg.melt) #view model stats

reg.frost = lm(NEE_springRMSE$RMSEall~NEE_springRMSE$frost.day) #run linear model
plot(NEE_springRMSE$RMSEall~NEE_springRMSE$frost.day) #plot data
abline(reg.frost, col="red") #add model line
summary(reg.frost) #view model stats

reg.length = lm(NEE_springRMSE$RMSEall~NEE_springRMSE$length) #run linear model
plot(NEE_springRMSE$RMSEall~NEE_springRMSE$length) #plot data
abline(reg.length, col="red") #add model line
summary(reg.length) #view model stats

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
par(mfrow=c(2,1))
data.check = data[match(out.compare_NEE$time, data$time),]
plot(data.compare_NEE$NEE~data.check$PAR_ARF, pch=16)
points(out.compare_NEE$NEE~data.check$PAR_ARF, col="red")
plot(data.compare_NEE$NEE~data.check$Temp_ARF, pch=16)
points(out.compare_NEE$NEE~data.check$Temp_ARF, col="red")
