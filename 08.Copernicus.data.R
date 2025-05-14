library (ncdf4) # read Copernicus data with nc extension 
library(terra)

setwd("/Users/anita/Desktop/Unibo/Courses/1st YEAR/1st SEMESTER/Spatial Ecology in R") #set working directory with the path R needs to find the document

soilm2023<-rast("c_gls_SSM1km_202311220000_CEURO_S1CSAR_V1.2.1.nc")
soilm2023
# no projection file
# reference system in 3d

plot(soilm2023)
# we're interested only in the first image, not in that with noise
plot(soilm2023[[1]])

cl<-colorRampPalette(c("red", "orange", "yellow"))(100)
plot(soilm2023[[1]], col=cl)

ext<-c(22,26,55,57) #min&max longitude (x), min&max latitude (y)
# we use this extent to crop another image
soilm2023crop<-crop(soilm2023,ext)
# let's see it
plot(soilm2023crop[[1]], col=cl)  
  
# another extent
ext1<-c(10,24,60,75)
soilm2023crop1<-crop(soilm2023,ext1)
plot(soilm2023crop1[[1]], col=cl) 


# ANOTHER IMAGE
setwd("/Users/anita/Desktop/Unibo/Courses/1st YEAR/1st SEMESTER/Spatial Ecology in R")
soilm2023_25<-rast("c_gls_SSM1km_202311250000_CEURO_S1CSAR_V1.2.1.nc")
soilm2023_25crop<-crop(soilm2023_25,ext1)
plot(soilm2023_25crop[[1]], col=cl) 
