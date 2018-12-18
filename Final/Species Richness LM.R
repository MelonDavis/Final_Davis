#------------------------------------------------------------------------------
#Creating a Species richness scatter plot and regression

#Load the Data
Invert.Rich <- read.csv("QuadRich.csv", na.strings = c("NA", "-"))
View(Invert.Rich)

#Create the plot
xlim.Rich <- range(Invert.Rich$WE)
ylim.Rich <- range(Invert.Rich$Species.Richness)
plot(Invert.Rich$WE, Invert.Rich$Species.Richness, xlim = xlim.Rich, 
     ylim = ylim.Rich, xlab = "Distance from HTL (ft)", 
     ylab = "Species Richness", pch = 19)
#Add regression line
m.m <- lm(Invert.Rich$Species.Richness ~ Invert.Rich$WE)
abline(m.m)

