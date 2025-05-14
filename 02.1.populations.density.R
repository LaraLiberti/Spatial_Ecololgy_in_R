# Codes related to population ecology

# Let's install spatstat, which allows us to make a SPATIAL POINT PATTERN ANALISYS
# A package is needed for point pattern analysis

install.packages("spatstat")
# QUOTES are needed to protect the package we want to install which is outside R

library(spatstat)
#the function library(spatstat) is used to check if the package has already been installed

# we are using data in GitHub, which is  outside R, in order to previously understand how they work

#DATA DESCRIPTION
# let's use datasets provided by spatstat, the bei data:
# data description: The dataset bei gives the positions of 3605 trees of 
# the species Beilschmiedia pendula (Lauraceae) in a 1000 by 500 metre 
# rectangular sampling region in the tropical rainforest of Barro Colorado Island


# plotting the data FROM SPATSTAT (see what it represents)
# as the points are too big for this area, we'll change their shape
plot (bei)

# changing dimension - cex
plot (bei, cex=.5)

plot(bei, cex=.2)

# why are the trees clumbed in some area?
# changing the symbol - pch
plot (bei, cex=2, pch=19)
#search the number of R symbols on the internet

# additional datasets
bei.extra
# it has two variables: elevation (elev) and gradient (grad). 
# They allow us to understand the distribution of bei datas

plot (bei.extra)
#here we have the raster file =! vector file

# let's use only part of the dataset: elev
bei.extra$elev  #$ sign links elevation to the dataset
elevation <- plot(bei.extra$elev)  
#elevation has been assignet to an homonimous object, simple to find

#second method to select elements
bei.extra[1] #take the fist element, so it's another way to isolate elevation
elevation2 <- bei.extra[[1]]
plot(elevation2)

# INTERPOLATION: passing from points to a continuous surface 
densitymap<-density(bei) #the densitymap gives us info about the distribution of pixel
plot(densitymap)

#Let's overlap  bei points to the densitymap
points(bei, cex=.2)

#Avoid pictures with a combination of blue, green and red colors as daltonic people can't see them

#Let's change the colors
# You can find them in this website:
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
cl <- colorRampPalette(c("black", "red", "orange", "yellow"))(100) #R is capital letters sensitive!!!
#100 represents the different colors that can be present between those chosen
plot(densitymap, col=cl)

#Use yellow colour in a proper way, cause it's the most impacting one

#the quality is much worse if we put a smaller number, like
cl <- colorRampPalette(c("black", "red", "orange", "yellow"))(4)
plot(densitymap, col=cl)

# MULTIFRAME (shows different plots at the same time)
par(mfrow=c(1,2)) #1 row and 2 columns, and they're part of an array
# and then the two plots 
plot(densitymap)
plot(elev)

par(mfrow=c(2,1)) #2 rows and 1 column, and they're part of an array
# and then the two plots 
plot(densitymap)
plot(elev)




