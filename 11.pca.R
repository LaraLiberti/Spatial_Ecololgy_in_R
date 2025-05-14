library (imageRy)
library(terra)
library(viridis)

im.list()
dev.off()

sent <- im.import("sentinel.png") #Similaun Glacier

pairs(sent)
viridisc <- colorRampPalette(viridis(7))(255)

# Pearson correlation index goes from -1 to +1. We see that there's perfect correlation between the visible bands, wile the NIR adds more info so there's no correlation with the others. 


# MULTIVARIATE ANALYSIS
# Measurement of R based variability with a single layer, which is chosen objectivle with Multivariate Analysis
# compact the 3 bands of sentinel into one to better visualize it

# we have already seen the DCA (detrended component analysis)
# now we will see the PCA (principal component analysis)
# the last row is nothing

sentpc <- im.pca2(sent)


# it says that the principal component will show the 71% of the variability, the second 53% etc.

pc1 <- sentpc$PC1 #we choose only the first principal component
plot(pc1, col=viridisc)

# calculating sd onto of pc1 in a 3x3 matrix
pc1sd3 <- focal(pc1, matrix(1/9,3,3), fun=sd)
plot(pc1sd3, col=viridisc)


# calculating sd onto of pc1 in a 7x7 matrix
pc1sd7 <- focal(pc1, matrix(1/49,7,7), fun=sd)
plot(pc1sd7, col=viridisc)



# multiframe

sd3 <- focal(nir, matrix(1/9,3,3), fun=sd)
sd7 <- focal(nir, matrix(1/49,7,7), fun=sd)

par(mfrow=c(3,3))
im.plotRGB(sent, 2,1,3)
# sd from the variability script:
plot(sd3, col=viridisc)
plot(sd7, col=viridisc)
plot(pc1, col=viridisc)
plot(pc1sd3, col=viridisc)
plot(pc1sd7, col=viridisc)
dev.off()

# stack all the sd layers
sdstack <- c(sd3, sd7, pc1sd3, pc1sd7)
plot(sdstack, colr = viridisc)

# change the names

names(sdstack) <- c("sd3", "sd7", "pc1sd3", "pc1sd7")
plot(sdstack, col = viridisc)

#focal can also be used for other statistics: mean, coefficent variation etc.
