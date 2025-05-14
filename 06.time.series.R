# TIME SERIES are series of data (images) scattered in time 
library(terra)
library(imageRy)
im.list()

#### CHANGES IN NITROGEN CONCENTRATION due to COVID, from January to March 2020
EN01<-im.import("EN_01.png") #European Nitrogen in January
EN13<-im.import("EN_13.png") #European Nitrogen in March
EN01
EN13

par(mfrow=c(2,1))
im.plotRGB.auto(EN01)
im.plotRGB.auto(EN13)

# Let's make the difference between the red bands of the first two images
dev.off()
diff=EN01[[1]]-EN13[[1]]
plot(diff)

# Reminder: blue and red or green and red aren't good for daltonic people
# So, let's change the colors

cl <- colorRampPalette(c('red','orange','yellow'))(100)  
plot(diff,col=cl) 
# red is higher in march, yellow is higher in january
# so we have a huge decrease in march (there's more yellow than red)
# we see that in many cities people stopped to use cars

##### CHANGES IN TEMPERATURE of Greenland ice sheet

dev.off()
im.list()
G2000<-im.import("greenland.2000.tif") #16 bits image
plot(G2000,col=cl) #temperature on the surface of the land
# red=almost perennial ice cap

G2005<-im.import("greenland.2005.tif")
G2010<-im.import("greenland.2010.tif")
G2015<-im.import("greenland.2015.tif")

par(mfrow=c(2,2))
plot(G2000,col=cl)
plot(G2005,col=cl)
plot(G2010,col=cl)
plot(G2015,col=cl)

# to make it more effective
cl1 <- colorRampPalette(c('black', 'blue','white','red'))(100) 
par(mfrow=c(2,2))
plot(G2000,col=cl1)
plot(G2005,col=cl1)
plot(G2010,col=cl1)
plot(G2015,col=cl1)

# it's the same as

dev.off()
stackG4<-c(G2000,G2005,G2010,G2015) #around 2005 there was the worst period
plot(stackG4, col=cl1)
# we see that the surface temperature in Greenland have increased and then decreased back, while that in the Nunavut has gradually increased
# also Island's temperature has decreased a little bit

# or only the first and the last
dev.off()
stackG2<-c(G2000,G2015)
plot(stackG2, col=cl1)

# Make the difference between the first and the last element
diffg<-stackG4[[1]]-stackG4[[4]]
# it's the same as
difg<-G2000-G2015
plot(diffg, col=cl1) 
# blue=temperature was lower in the past 
# so there's a lot of increase in temperatures, therefore so decrease in ice sheet
# red= temperature was higher in the past

# Make a RGB plot using different years
dev.off()
im.plotRGB(stackG4, r=1,g=2,b=3)
# red = temperature was lower in the past (external part)
# green = temperature similar to the past
# blue = temperature was higher in the past (central part)
