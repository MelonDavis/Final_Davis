#----Creating Elevation Map----------------------------------------------------
#Set working directory
getwd()
setwd("C:/Users/Ben Shanafelt/Documents/Data Analysis in R/Projct Folder/Final_Davis/Final/")
#Load Data
quad.coords <- read.csv("QuadCoords.csv")

#Set x and y coordinates for the quadrats
qcx <- quad.coords$NS
qcy <- quad.coords$WE

#Create an empty plot, with the x and y limits
plot(NA, bty = 'n', pch = '', ylab = 'Distance from High Tide Line (WE)', 
     xlab = 'Distance along NS transect', xlim = c(0, 100), 
     ylim = c(0, 200))

#Create a box around the plot
box(which = "plot", lty = "solid")

#add in ablines for each section
abline(50, 0)
abline(100, 0)
abline(150, 0)
abline(200, 0)
abline(0, 0)

#Add a point for each quadrat
points(x = qcx, y = qcy, pch = 16, cex = 1, col = "black")
