# SATELLITE DATA VISUALIZING
# Where to store additional packages, not present in R? 
# GitHub (not connected to R: it's not controlled) or CRAN (directly conencted to R: it's controlled)

# to install them from CRAN (Coomprehensive R Archive Network), the function is: install.packages()
install.packages("devtools")

# to open the packages, the function is always: library()
library (devtools) 

# to install them from GitHub, the function is: install_github()
install_github("ducciorocchini/imageRy") # devtools package allows to download packages outside CRAN
library (imageRy)

# to see all the possible images from the imageRy package, the function is: im.list()

# to import one image, the function is: im.import()
b2 <- im.import("sentinel.dolomites.b2.tif")

# Information provided from the image (called Reference System): 
# Word Geodetic System, Universal Transverse Mercator, Coordination of the point + the emisphere (N or S)
# to know more, you can go to Sentinel-2 in Wikipedia

clb <- colorRampPalette(c("dark grey", "grey", "light grey")) (100)
plot(b2,col=clb)

c1 <- colorRampPalette(c("black", "grey", "white")) (100)
c2 <- colorRampPalette(c("purple", "pink", "white")) (100)
c3 <- colorRampPalette(c("dark green", "light green", "white")) (100)
c4 <- colorRampPalette(c("blue", "light blue", "white")) (100)

par(mfrow=c(2,2))
plot(b2,col=c1)
plot(b3,col=c2)
plot(b4,col=c3)
plot(b8, col=c4) 


###### # STACK IMAGES
# they're a function that plots all the selected images together, one over the other (in this case, four bands all together)
# RGB SPACE = Red, Green and Blue components that build other colours by overlapping
# 1.BLUE
b2 <- im.import("sentinel.dolomites.b2.tif") 

# 2.GREEN
b3<-im.import("sentinel.dolomites.b3.tif") 

# 3.RED
b4 <- im.import("sentinel.dolomites.b4.tif") 

# 4.NIR
b8<-im.import("sentinel.dolomites.b8.tif") 

stack_sent <- c(b2,b3,b4,b8)

im.plotRGB(stack_sent, r = 3, g = 2, b = 1) # our view

im.plotRGB(stack_sent, 4, 3, 2) # with infrared: vegetation becomes red, we get more information

# CHANGE THE POSITION OF THE NIR
im.plotRGB(stack_sent, r=3, g=4, b=2)
im.plotRGB(stack_sent, r=3, g=2, b=4)


# CORRELATIONS BETWEEN THE BANDS
pairs(stack_sent)
