#first upload and viewdata 

data = read.csv("OriginalData_NOTfilled.csv") #choose file from directory
names(data) = c("time", "year", "DOY","Temp_T", "PAR_T", "Temp_ARF", "PAR_ARF")
head(data) #view table

#plot data
par(mfrow=c(2,1)) 
plot(data$Temp_T~data$time, type="l", ylab = "Daily AVG Temp (C)", xlab="Time (days)") 
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16, cex=0.5)
plot(data$PAR_T~data$time, type="l", ylab = "Daily AVG PAR (umol m-2 s-1)", xlab="Time (days)")
points(data$PAR_ARF~data$time, col="blue", pch=16, cex=0.5)
#ARF data is missing winter measurements - want to fill this in

#want to plot to determine relationship, and then fill in missing data in ARF data
#start with temperature
linmod.t = lm(data$Temp_ARF~data$Temp_T + 0) #linear model, set intercept = 0
summary(linmod.t) #summary
slope.t = summary(linmod.t)$coefficients[1,1] #slope value

par(mfrow=c(1,1))
plot(data$Temp_ARF~data$Temp_T, ylab = "ARF", xlab="Toolik", main="Temperature", pch=16, col="red")
abline(linmod.t, lwd = 2) #linear regression line

#do the same thing for PAR data
linmod.p = lm(data$PAR_ARF~data$PAR_T + 0)
summary(linmod.p)
slope.p = summary(linmod.p)$coefficients[1,1]

par(mfrow=c(1,1))
plot(data$PAR_ARF~data$PAR_T, ylab = "ARF", xlab="Toolik", main="PAR", pch=16, col="blue")
abline(linmod.p, lwd = 2)

#fill in missing data - NEED TO FIGURE OUT HOW TO CARRY UNCERTAINTY THROUGH THIS???

for(i in 1:length(data$Temp_ARF)){
  
  if(is.na(data$Temp_ARF[i])==TRUE) {
    data$Temp_ARF[i] = data$Temp_T[i]*slope.t 
  }
  
  if(is.na(data$PAR_ARF[i])==TRUE) {
    data$PAR_ARF[i] = data$PAR_T[i]*slope.p
  }
}

#need to convert units of PAR
data$PAR_ARF = data$PAR_ARF*(1E-6)*86400
data$PAR_T = data$PAR_T*(1E-6)*86400

#check output to make sure it all lines up
par(mfrow=c(2,1))
plot(data$Temp_T~data$time, type="l", ylab = "Daily AVG Temp (C)", xlab="Time (days)")
abline(h=0)
points(data$Temp_ARF~data$time, col="red", pch=16, cex=0.5)
plot(data$PAR_T~data$time, type="l", ylab = "Daily AVG PAR (mol m-2 s-1)", xlab="Time (days)")
points(data$PAR_ARF~data$time, col="blue", pch=16, cex=0.5)

write.csv(data, "InputData_Processed.csv") #added the updated data to the working directory so that it is easy to access - won't have to do any of the above steps again
