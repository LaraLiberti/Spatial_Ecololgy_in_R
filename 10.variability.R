# Measurement of R based variability
# there are different indices in statistics: the most simple thing in our case was to measure the standard deviation, that is how much the different data are divergin from the mean 
# we measured it with the focal() function

library (imageRy)
library(terra)
library(viridis)

im.list()

sent <- im.import("sentinel.png") # Similaun Glacier

# band 1 = NIR
# band 2 = red
# band 3 = green

im.plotRGB(sent, r = 1, g = 2, b = 3)

# snow in white, lake in black

im.plotRGB(sent, r = 2, g = 1, b = 3)

nir <- sent[[1]]
plot(nir) # 8 bit image as it distributed in a range between 0 and 250

# MOVING WINDOW APPROACH FOR DIVERSITY ASSESSMENT
# we are going to calculate the standard deviation (sd) of windows of pixels, one at the time
# in the end we would have passed the window in the whole image, calculating each time the sd value of the central pixel of the window 

# calculate the standard deviation with the function: focal()

sd3 <- focal(nir, matrix(1/9,3,3), fun=sd) # avoid givid the name sd without numbers cause it can be confused with the value of the standard deviation

# use of the nir band, matrix describes the dimension of the moving window. it is composed by 9 pixels, so they go from 1 to 9 (written as 1/9). In general we use squared matrices. The function calculated is the standard deviation (sd).

plot(sd3)

cl<-colorRampPalette(viridis(7))(255) # the 7th palette of viridis, with 255 different colors used

plot(sd3, col=cl) # northwest part is the area where the variability is higher

# let's calculate the variability in a 7x7 moving window
sd7 <- focal(nir, matrix(1/49,7,7), fun=sd)
plot(sd7, col=cl) 
# sd in a smaller window will give a local calculation and see subtile differences
# with larger windows, the resulting picture will include additional pixels, so it will be less precise

par(mfrow=c(1,2))
plot(sd3, col=cl)
plot(sd7, col=cl)

# original image plus the 7x7 sd
par(mfrow=c(1,2))
im.plotRGB(sent, r = 2, g = 1, b = 3)
plot(sd7, col=cl) 
# the line on the top left reflects a lot of variability; 
# it can be due to snow or a clowd better recognized in the nir picture. 
# Other times it can be due to geological or botanical variability
