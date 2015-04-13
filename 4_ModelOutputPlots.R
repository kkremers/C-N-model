##########################PLOT MODEL OUTPUTS###########################

#plot model inputs
par(mfrow=c(2,1), mar=c(4,4,0.5,2))
plot(data$Temp_ARF~data$time, type="l", ylab = "Daily Max Temp (C)", col="red", xlab="")
abline(h=0)
plot(data$PAR_vis~data$time, type="l", ylab = "Daily Plant Avail. PAR (mol m-2 day-1)", col="blue", xlab = "Time (days)")



#plot pools
par(mfrow=c(3,2), mar=c(4,4,1,2))
plot(out$Biomass_C~out$time, type="l", col="springgreen3", main = "Biomass C", xlab="", ylab="g C m-2")
plot(out$Biomass_N~out$time, type="l", col="springgreen3",  main = "Biomass N", xlab="", ylab="g N m-2", lty=2)
plot(out$Litter_C~out$time, type="l", col="orange", main = "Litter C", xlab="", ylab="g C m-2")
plot(out$Litter_N~out$time, type="l", col="orange", main = "Litter N", xlab="", ylab="g N m-2", lty=2)
plot(out$SOM_C~out$time, type="l", col="red", main = "SOM C", xlab="Time (days)", ylab="g C m-2")
plot(out$SOM_N~out$time, type="l", col="red", main = "SOM N", xlab="Time (days)", ylab="g N m-2",lty=2)
plot(out$Available_N~out$time, type="l", col="green", main = "Available N", xlab="Time (days)", ylab="g N m-2",lty=2)






#see how well data matches
#to compare on 1:1 line with data, need to select only points for which data is available
data.compare=read.csv("ALLData_Assim.csv")
data.compare=data.compare[,1:5]
data.compare=data.compare[complete.cases(data.compare),]
head(data.compare)
out.compare = out[match(data.compare$time, out$time),]

par(mfrow=c(2,2), mar=c(2,4,2,2))
plot(out$GPP~out$time, col="azure4", pch=18, ylab="GPP (gC m-2 day-1)", xlab="", type="l")
points(data$GPP, col="blue", pch=18, cex=0.8)
plot(data.compare$GPP, out.compare$GPP)
abline(0,1, col="red")

plot(out$LAI~out$time, col="azure4", pch=18, ylab="LAI (m2 leaf m-2 ground)", xlab="", type="l")
points(data$LAI, col="blue", pch=18, cex=0.8)
plot(data.compare$LAI, out.compare$LAI)
abline(0,1, col="red")

plot(-out$Re~out$time, col="azure4", pch=16, ylim=c(-5,0), xlab="Time (days)", ylab="Re (gC m-2 day-1)", type="l")
points(-data$Re, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare$Re, out.compare$Re)
abline(0,1, col="red")

plot(out$NEE~out$time, col="azure4", pch=18, ylim=c(-3,2), xlab="Time (days)", ylab="NEE (gC m-2 day-1)", type="l")
points(data$NEE, col="blue", pch=16, cex=0.6)
abline(h=0)
plot(data.compare$NEE, out.compare$NEE, ylim=c(-4, 1))
abline(0,1, col="red")






par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(data$GPP~data$PAR_vis, pch=16, ylab="GPP", xlab="PAR_vis")
points(out$GPP~data$PAR_vis, col="red")

plot(data$LAI~data$Temp_ARF, pch=16, ylab="LAI", xlab="Temperature")
points(out$LAI~data$Temp_ARF, col="red")

plot(data$Re~data$Temp_ARF, pch=16, ylab="Re", xlab="Temperature")
points(out$Re~data$Temp_ARF, col="red")

plot(data$NEE~data$Temp_ARF, pch=16, ylab="NEE", xlab="Temperature")
points(out$NEE~data$Temp_ARF, col="red")




#plot CUE and LAI
par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$Uptake~out$Available_N, xlab = "Available N (g N m-2)", ylab = "Uptake (g N m-2 day-1)")
plot(out$Uptake~out$time, type="l",  xlab = "Time (days)", ylab = "Uptake (g N m-2 day-1)")

par(mfrow=c(2,1), mar=c(4,4,2,2))
plot(out$s.GDD~data$TempPos, xlab = "TempPos", ylab = "Scalar (s.GDD)")
plot(out$LAI~data$TempPos, xlab = "TempPos", ylab = "LAI (m2 m-2)")
plot(out$LAI~out$Biomass_N, xlab = "Biomass_N (gN m-2)", ylab = "LAI (m2 m-2)")
