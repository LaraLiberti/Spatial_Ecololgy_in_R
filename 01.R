# Here I can write anything I want

# R as a caluclator
3+4

# Assign to an object
zima <- 2 + 3

duccio <- 5 + 3

# Calculations
zima * duccio
final <- zima * duccio


##### Array
# they're functions
# fuctions always have parenthesis and inside them there're the arguments

# Examples
sophi <- c(10,20,30,50,70) # microplastics
paula <- c(100, 500, 600, 1000, 2000) #people

# we can plot them together
plot(paula, sophi, xlab="number of people", ylab="microplastics")

# we can also previously assign paula and sophi to some objects
people <- paula
microplastics <- sophi
plot (people, microplastics, pch=19, cex=2, col="blue") # pch gives the shape of the symbols in R, cex represents the size, col for the color

# we can find the symbols in this site:
# http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
