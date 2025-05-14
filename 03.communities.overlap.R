# MULTIVARIATE ANALISYS of how species are related in TIME 
install.packages("overlap")
library(overlap)

data(kerinci) # data from Kerinci-Seblat National Park in Sumatra, Indonesia
kerinci
head(kerinci)
summary(kerinci)

# Selecting the first species
tiger <- kerinci[kerinci$Sps=="tiger",] # Sps=species

# Selecting the tigers time
timetig <- kerinci$Time*2*pi 
# we multiply the the linear time to 2 pi greek to have a time in radiants

densityPlot(timetig, rug=TRUE)

# Selecting the macaque
macaque <- kerinci[kerinci$Sps=="macaque",] # Sps=species

# Selecting the macaque time
timemac <- macaque$Time*2*pi 

densityPlot(timemac, rug=TRUE)

overlapPlot(timetig,timemac)
