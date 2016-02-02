#This is to determine relationships that will allow us to convert tower NDVI to unispec NDVI to MODIS NDVI


#first load data
data = data.frame(read.csv("NDVI_Compare.csv"))
head(data)

#look at relationships with NDVI NOT AVERAGED and MODIS INTERPOLATED
Tower_Unispec = lm(data$NDVI_UniSpec~data$NDVI_7avg)
summary(Tower_Unispec)
plot(data$NDVI_7avg,data$NDVI_UniSpec, pch=16)
abline(Tower_Unispec)


#load other data
data = data.frame(read.csv("NDVI_Compare2.csv"))
head(data)

Tower_Unispec2 = lm(data$NDVI_UniSpec~data$NDVI_Rad)
summary(Tower_Unispec2)
plot(data$NDVI_Rad,data$NDVI_UniSpec, pch=16)
abline(Tower_Unispec2)

Unispec_MODIS = lm(data$NDVI_MODIS~data$NDVI_UniSpec)
summary(Unispec_MODIS)
plot(data$NDVI_UniSpec,data$NDVI_MODIS, pch=16)
abline(Unispec_MODIS)
abline(0,1, col="red")

