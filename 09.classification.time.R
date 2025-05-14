# Estimate the qualitative and quantitative difference between two images taken at different times
# We need to transform images in classes (land cover, land use, etc.)

# Satellite image of the Alps: green snow, blue pastures, green forests, pink villages
# We can compare quantitative (points in the graph) and qualitative (images RGB) data, by taking a picture in the image and analyze the waves it reflects
# Thay we try to guess the element according to the reflectance: water for example reflects NIR and red

# Pixels = training sites, expressing the main classes or clusters. They're set of individual objects having the same features. In this case the main classes are agricutural areas, forests, etc.

# How to classify pixels? From : we can estimate if the pixel represents an urban area or a forest using the the minimum distance from the class we think it belongs

# CLASSIFYING SATELLITE IMAGES AND ESTIMATING THE AMOUNT OF CHANGE
library(terra)
library(imageRy)
im.list()
#S un pillars are sun regions with high energy

# https://www.esa.int/Science_Exploration/Space_Science/Solar_Orbiter/Solar_Orbiter_s_first_views_of_the_Sun_image_gallery

sun<-im.import("Solar_Orbiter_s_first_views_of_the_Sun_pillars.jpg")
# instant RGB picture, with 3 levels of energy: yellow (higher), brown, black. So, we expect 3 cluters.
# We have to explain to the softer the numbers of the cluster

sunc<- im.classify(sun, num_clusters = 3)
# We should reach similar images, but not the same. The random set of pixel makes every plot different in colors but not in shape.

# We want to measure the amount of error

m1992<- im.import("matogrosso_l5_1992219_lrg.jpg")
m2006<- im.import("matogrosso_ast_2006209_lrg.jpg")
plotRGB(m1992)

m1992c<-im.classify(m1992, num_clusters=2) # amount of classes that we want to see in the image
plot(m1992c) # 3 replicate of the same image: white class of forest, green human intervention

# Classes: forest = 1, human = 2
m2006c<-im.classify(m2006, num_clusters=2)
plot(m2006c)

par(mfrow=c(1,2))
plot(m1992c[[1]]) # DOUBLE PARENTHESIS to plot choose only the first image
plot(m2006c[[1]])

# What is the proportion of forest and human classes? function: freq() (calculates the pixels from a certain classes)
f1992<-freq(m1992c[[1]])

# Total number of pixels? function: ncell()
tot1992<-ncell(m1992c)
tot1992

# Percentage? 
p1992<-f1992*100/tot1992
p1992

# forest = 83%, human = 17%

# Percentage of 2006
f2006<-freq(m2006c[[1]])
tot2006<-ncell(m2006c)
p2006<-f2006*100/tot2006
p2006

# forest = 45%, human = 55%

# Building the final table, which means a graph with all this data -> function: data
# We need to build the columns
class<- c("forest", "human")
y1992<- c(83, 17) # forest and humans in 1992
y2006<- c(45, 55) # forest and humans in 2006

# And to assign them to the dataframe
tabout <- data.frame(class, y1992, y2006)
tabout

# And build a graph
library(ggplot2)
library(patchwork) # allows to put several graphs together

p1<- ggplot(tabout, aes(x=class, y=y1992, color=class)) + geom_bar(stat="identity", fill="white")
p2<- ggplot(tabout, aes(x=class, y=y2006, color=class)) + geom_bar(stat="identity", fill="white")
# bar plot with the tabout class, aestethics will be the class

p1+p2
