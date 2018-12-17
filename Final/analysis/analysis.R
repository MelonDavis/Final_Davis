#

#===== [Strip Chart] ====

#defines limitiations as objects first
xlim.1 <- range(seq(0.5, 8.5, 2))
ylim.1 <- range(inv.d$Number.of.individuals)

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




