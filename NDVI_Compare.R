#This is to determine relationships that will allow us to convert tower NDVI to unispec NDVI to MODIS NDVI


#first load data
data = data.frame(read.csv("NDVI_Compare.csv"))
head(data)


#look at relationships with NDVI NOT AVERAGED and MODIS INTERPOLATED
Tower_Unispec = lm(data$UniSpec~data$Tower)
summary(Tower_Unispec)
plot(data$Tower,data$UniSpec, pch=16)
abline(Tower_Unispec)

Unispec_MODIS = lm(data$MODIS~data$UniSpec)
summary(Unispec_MODIS)
plot(data$UniSpec,data$MODIS, pch=16)
abline(Unispec_MODIS)

