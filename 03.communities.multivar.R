#In communities, species are overlapping in space and time
# MULTIVARIATE ANALISYS of how species are related in SPACE 

#Vegetation Analisys
install.packages("vegan")
library(vegan)
data(dune) 
# Dune is a data frame of observations of 30 species at 20 sites. 
# The species names are abbreviated to 4+4 letters.

dune #to see the dataset
#or
head(dune) #just the first 6 rows
tail(dune) #just the last 6 rows
#we see a matrix of amout of individuals present in every plot

ord<-decorana(dune) #decorana function from the package vegan gives the Detrended Correspondace Analysis
# DCA is a multivariate statistical technique used by ecologists to find the main factor or gradient in species-rich but sparse data matrices that tipify ecological community data. 
#we have to know the length of the new axes

ldc1 = 3.7004 #length decorana 1
ldc2 = 3.1166 
ldc3 = 1.30055 
ldc4 = 1.47888
total = ldc1 + ldc2 + ldc3 + ldc4

pldc1 = ldc1 * 100/total #percentage of every axes
pldc2 = ldc2 * 100/total
pldc3 = ldc3 * 100/total
pldc4 = ldc4 * 100/total

pldc1
pldc2
pldc3
pldc4

#as pldc1 + pldc2 represent the 70% of the total, we can also keep them only

plot(ord) 
# names represent species (we see coupling species), numbers represent plots: everything is depicted in a new dimention made by dca1 and 2
# from the table we can understand the environments
