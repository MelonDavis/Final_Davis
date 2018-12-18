#==== [Calculate total of each quad] ======

#object to store sum of each quadrant (number = total num of quads)
sum.quad <- rep(NA, 30)

all <- unique(allquad$Location)

#this should work but i think i need to collapse by quad seperately
for (i in 1:30){
  
  sec <- all[i]
  sum.quad[i] <- sum(allquad$Number.of.individuals[allquad$Location == sec])
  
}

quadttls <- data.frame(substr(all, 1, 1), all, sum.quad)
colnames(quadttls) <- c("Section", "Location", "Number.of.ind")


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

#sample size of each site
n <- quadnum


for (i in 1:4) {
  
test <- labels[i]

#draw n points with a jitter of 5 in scposition (matching with labels) at the
  #y coordinates of number of individuals when the section value is equal to
  #position i of test; do this for each value of i
points(jitter(rep(scposition[i], n[i]), 5), 
       quadttls$Number.of.ind[quadttls$Section == test], pch = 19)

}


#hexa code = #rrggbbAA; AA determines transparency

