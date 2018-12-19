#===== [Analysis] ======

#---- Anova ----

#ind ttls
#For our ANOVA we are removing section E
#set rows that have section E
secE <- which(quadttls.d$Section == "E")
#save a new frame without those rows
ADHttls <- quadttls.d[-c(secE),] 

#perform an ANOVA test on the new data frame
ttls.aov <- aov(Number.of.ind ~ Section, data = ADHttls)
summary(ttls.aov)

#start pdf for total residuals
pdf(paste(p.final[3], "ttls_res.pdf", sep = ""), width = 6, height = 4)

#plot residuals to make sure they don't violate 
plot(ttls.aov, 1)

dev.off()

#richness

#For our ANOVA we are removing section E
#set rows that have section E
secE <- which(rich.quad$Section == "E")
#save a new frame without those rows
ADHrich <- rich.quad[-c(secE),] 

#perform an ANOVA test on the new data frame
rich.aov <- aov(Species.Richness ~ Section, data = rich.quad)
summary(rich.aov)

#start a pdf for richness residuals
pdf(paste(p.final[3], "rich_res.pdf", sep = ""), width = 6, height = 4)

#plot residuals to make sure they don't violate 
plot(rich.aov, 1)
?aov()

dev.off()

#===== [Strip Charts] =====
#---- total ind ----

#start pdf of the strip chart of total number of individuals
pdf(paste(p.final[3], "box_indttls.pdf", sep = ""), width = 6, height = 4)

#defines limitiations as objects first
xlim.1 <- range(seq(0.5, 8.5, 2))
ylim.1 <- range(quadttls.d$Number.of.ind)

#define color scheme as object
sec.col <- c("#27d76f60", "#cd7ce750", "#f2a42450", "#0b00d150")

#create empty plot
plot(NA, xlim = xlim.1, ylim = ylim.1, xaxt = 'n', xlab = "Section", 
     ylab = "# of Ind", main = "Section Total Ind.", las = 1)
#xaxt = 'n' means supress the x axis

#generate labels and position for x axis (sections)
labels <- c(unique(inv.d$Section))
scposition <- seq(1.5, 8.5, 2)

#for loop to draw the labels, 1:4 is how many will be drawn
for (i in 1:4) {
  #on axis 1 create a label at position i with label i
  axis(1, c(scposition[i]), c(labels[i])) 
}

#Now I want to plot the data
for (i in 1:4) {
  
  #it has trouble reading [] within [] so i started by making an object that 
  #would be the label of current position of the loop
  templab <- labels[i]
  #assign the number of individuals of the current section to an object
  tempnumind <- quadttls.d$Number.of.ind[quadttls.d$Section == templab]
  
  #define the sample size as the length of number of sums for each section
  n <- length(tempnumind)
  
  #draw n points with a jitter of 5 in scposition (matching with labels) at the
  #y coordinates of number of individuals when the section value is equal to
  #position i of test; do this for each value of i
  points(jitter(rep(scposition[i], n), 3), tempnumind, pch = 19, col = 
           sec.col[i])
  
  #Now we want to plot the median and the standard error
  #Store summary in temporary object to use to reference y coordinates
  tempsum <- summary(tempnumind)
  
  #First lets draw our median line
  
  #I only want my lines to last for the category (not whole plot area)
  #My range is the label position + and - 1
  linelim <- range(scposition[i] - 0.25, scposition[i] + 0.25)
  
  #can draw segments whose x positions are based on category limit and y position
  #is the mean
  segments(x0 = linelim[1], x1 = linelim[2], 
           y0 = tempsum[3], y1 = tempsum[3])
  
  #We also want to make a box displaying the interquartile range
  
  #Linelim defines border of category
  xx <- c(linelim[2], linelim[1], linelim[1], linelim[2])
  
  #Tempsum defines IQR from 1st quartile to 3rd quartile
  yy <- c(tempsum[2], tempsum[2], tempsum[5], tempsum[5])
  
  #draw polygon
  polygon(xx, yy, density = 0)
  
  #Finally, we want to display standard error
  #Standard error = standard dev / square root of sample size
  st.er <- sd(tempnumind) / sqrt(n)
  
  #Can add standard error bars
  #segments(x0 = scposition[i], x1 = scposition[i], y0 = (tempsum[3] - st.er), 
  #y1 = (tempsum[3] + st.er))
  
}

dev.off()

#**and then generate a plot for each group so you can see the spread


#---- richness ----

#start pdf of the strip chart of richness
pdf(paste(p.final[3], "box_richttls.pdf", sep = ""), width = 6, height = 4)

#We can use the same code as above only need to redefine y lim and switch data 
  #frame
ylim.3 <- range(rich.quad$Species.Richness)

#create empty plot
plot(NA, xlim = xlim.1, ylim = ylim.3, xaxt = 'n', xlab = "Section", 
     ylab = "Richness", main = "Section Richness", las = 1)
#xaxt = 'n' means supress the x axis

#for loop to draw the labels, 1:4 is how many will be drawn
for (i in 1:4) {
  #on axis 1 create a label at position i with label i
  axis(1, c(scposition[i]), c(labels[i])) 
}

for (i in 1:4) {

  #it has trouble reading [] within [] so i started by making an object that 
  #would be the label of current position of the loop
  templab <- labels[i]
  #assign the number of individuals of the current section to an object
  tempnumind <- rich.quad$Species.Richness[quadttls.d$Section == templab]
  
  #define the sample size as the length of number of sums for each section
  n <- length(tempnumind)
  
  #draw n points with a jitter of 5 in scposition (matching with labels) at the
  #y coordinates of number of individuals when the section value is equal to
  #position i of test; do this for each value of i
  points(jitter(rep(scposition[i], n), 3), tempnumind, pch = 19, col = 
           sec.col[i])
  
  #Now we want to plot the median and the standard error
  #Store summary in temporary object to use to reference y coordinates
  tempsum <- summary(tempnumind)
  
  #First lets draw our median line
  
  #I only want my lines to last for the category (not whole plot area)
  #My range is the label position + and - 1
  linelim <- range(scposition[i] - 0.25, scposition[i] + 0.25)
  
  #can draw segments whose x positions are based on category limit and y position
  #is the mean
  segments(x0 = linelim[1], x1 = linelim[2], 
           y0 = tempsum[3], y1 = tempsum[3])
  
  #We also want to make a box displaying the interquartile range
  
  #Linelim defines border of category
  xx <- c(linelim[2], linelim[1], linelim[1], linelim[2])
  
  #Tempsum defines IQR from 1st quartile to 3rd quartile
  yy <- c(tempsum[2], tempsum[2], tempsum[5], tempsum[5])
  
  #draw polygon
  polygon(xx, yy, density = 0)
  
  #Finally, we want to display standard error
  #Standard error = standard dev / square root of sample size
  st.er <- sd(tempnumind) / sqrt(n)
  
  segments(x0 = scposition[i], x1 = scposition[i], y0 = (tempsum[3] - st.er), 
           y1 = (tempsum[3] + st.er))
}

dev.off()

