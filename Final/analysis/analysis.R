#==== [Calculate total of each quad] ======

#object to store sum of each quadrant (number = total num of quads)
sum.quad <- rep(NA, 30)

#object that has each of the section/quad labels
all <- unique(allquad$Location)

#for each of the unique labels of section/quad take the sum of the number of 
  #individuals when the row == the unique section/quad
for (i in 1:30){
  
  sec <- all[i]
  sum.quad[i] <- sum(allquad$Number.of.individuals[allquad$Location == sec])
  
}

#create data frame of quad totals with section, location, and sum of individuals
quadttls.d <- data.frame(substr(all, 1, 1), all, sum.quad)
colnames(quadttls.d) <- c("Section", "Location", "Number.of.ind")

#===== [Strip Chart] ====

#defines limitiations as objects first
xlim.1 <- range(seq(0.5, 8.5, 2))
ylim.1 <- range(quadttls.d$Number.of.ind)

#create empty plot
plot(NA, xlim = xlim.1, ylim = ylim.1, xaxt = 'n', xlab = "group", 
     ylab = "# of ind", las = 1)
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

#sample size of each site
n <- quadnum
#colors for different groups:
sec.col <- c("#27d76f60", "#cd7ce750", "#f2a42450", "#0b00d150")

#Create temporary objects for storage within for loop
tempsum <- NA
tempnumind <- c(rep(NA, 9)) #there will be a maximum of nine categories
tempmean <- NA
lowy <- NA
upy <- NA

xx <- c(rep(NA, 4))
yy <- c(rep(NA, 4))

for (i in 1:4) {
  
  #it has trouble reading [] within [] so i started by making an object that 
    #would be the label of current position of the loop
  templab <- labels[i]
  #assign the number of individuals of the current section to an object
  tempnumind <- quadttls.d$Number.of.ind[quadttls.d$Section == templab]
  
  #draw n points with a jitter of 5 in scposition (matching with labels) at the
    #y coordinates of number of individuals when the section value is equal to
    #position i of test; do this for each value of i
  points(jitter(rep(scposition[i], n[i]), 3), tempnumind, pch = 19, col = 
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
  lowy <- tempsum[3] - (sd(tempnumind)/2)
  upy <- tempsum[3] + (sd(tempnumind)/2)
  segments(x0 = scposition[i], x1 = scposition[i], y0 = lowy, y1 = upy )
  
}

#***why doesn't this work?
pdf(paste(p.final[3], "box_indttls.pdf", sep = ""), width = 6, height = 4)

#**and then generate a plot for each group so you can see the spread









