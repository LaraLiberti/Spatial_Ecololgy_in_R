# VEGETATION INDEX in 1992 and 2006 to see the evolution of the area
install.packages("ggplot2")
install.packages("viridis")
library(ggplot2)
library(viridis)
library(terra)
library(imageRy)
im.list()

m1992<- im.import("matogrosso_l5_1992219_lrg.jpg") # image from satellite LANDSAT (1962) # processed image, where bands 1=NIR, 2=RED, 3=GREEN
im.plotRGB(m1992, r=1, g=2, b=3) # in this way, the NIR bands will be depicted in red: they're typical of an healthy vegetation
# the RED ones will be represented in green: they're typical of unhealthy or absent vegetation
# and the GREEN one will be painted in blue

im.plotRGB(m1992, r=2, g=1, b=3)
im.plotRGB(m1992, r=2, g=3, b=1)

m2006<-im.import("matogrosso_ast_2006209_lrg.jpg")
im.plotRGB(m2006,r=2, g=3, b=1)

# import the 2006 image
m2006 <- im.import("matogrosso_ast_2006209_lrg") # another processed image, where bands 1=NIR, 2=RED, 3=GREEN
im.plotRGB(m2006, r=2, g=3, b=1)
im.plotRGB(m2006, r=2, g=1, b=3) #we see the result of deforestation

#build a multiframe with 1992 and 2006 images

par(mfrow = c(1, 2)) 
im.plotRGB(m1992, r=2, g=3, b=1)
im.plotRGB(m2006, r=2, g=3, b=1)
dev.off()

plot(m1992[[1]])
# In this picture, the range of reflectance goes from 0 to 255 (pink-green colors)
# Reflectance is the ratio between reflected and incident radiant flux 
# One bit of info can be 0 or 1 (BINARY CODE)
# The formula to calculate the amount of information every bit gives is:
# 2 raise to the power of n, where n is the number of bits.
# Ex: 4 bits will have 16 bits of information. 
# In this picture we have up to 8 bits, hence, up to 255 bits of reflectance (2 raised to power 8).

# The difference between the two bands give us the DVI.
# DVI = NIR - RED, where bands: 1=NIR, 2=RED, 3=GREEN
dvi1992 = m1992 [[1]] - m1992 [[2]] #dvi of 1992 is the difference between band 1 and 2
plot(dvi1992)

# now changing the palette
cl <- colorRampPalette (c("dark blue", "yellow", "red", "black"))(100)
plot(dvi1992, col=cl)
#everything that's dark red is healthy, yellow and blue is bad from the vegetation point of view as it represents bare soil.

# exercise DVI of 2006
dvi2006 = m2006[[1]] - m2006[[2]]
plot(dvi2006)
cl <- colorRampPalette (c("dark blue", "yellow", "red", "black"))(100)
plot(dvi2006, col=cl) #plotting the DVI of 2006 with the same coloring palette.

# NDVI = NIR - RED/ NIR + RED

ndvi1992 = ( m1992 [[1]] - m1992 [[2]] )/ (m1992[[1]] + m1992[[2]])
#also
ndvi1992 =  dvi1992/ (m1992[[1]] + m1992[[2]])
plot(ndvi1992, col=cl)

#NDVI for 2006

ndvi2006 = ( m2006 [[1]] - m2006 [[2]] )/ (m2006[[1]] + m2006[[2]])
ndvi2006 =  dvi2006/ (m2006[[1]] + m2006[[2]])
plot(ndvi2006, col=cl)

# plot them together 1992 and 2006, both ranging from -1 to 1. forest on right and cultivated on left

par(mfrow = c (1,2))
plot(ndvi1992, col=cl)
plot(ndvi2006, col=cl)

# speeding up the calculation of the NDVI, the function is: im.ndvi()

ndvi2006a <- im.ndvi (m2006, 1, 2) 
plot(ndvi2006a, col=cl) 
