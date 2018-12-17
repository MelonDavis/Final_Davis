#Clean

#====== [Load Clean] =====

#Data was cleaned in excel to match coordinate data (distance from high tide)
  #and results of each quadrat

#import data. NAs were entered as "-" so include transforming them into NA
inv.d <- read.csv(file.choose("invertebrate"), stringsAsFactors = FALSE, 
                  strip.white = TRUE, na.strings = c("NA","-"))

tail(inv.d) #successfully transformed dashes to NA

#----- Add Zero Quads ----

#We can see that data has only been entered for quadrats in which there were 
  #species, however if we want to represent all the data gathered we need to 
  #include the result of 0 for those quadrats w/ no species found 

#import coordinate data for every quadrat
coor.d <- read.csv(file.choose("coordinate"), stringsAsFactors = FALSE, 
                   strip.white = TRUE, na.strings = c("NA","-"))

head(coor.d)

#I want to start by making a new column of the pasted section and quadrat values
inv.d[,16] <- paste(inv.d$Section, inv.d$Quadrat, sep = "")
head(inv.d) #looks good!

nrow(inv.d) #118

#generate column of all quad values

#we start by defining how many quadrats were in each site
sitelab <- c("A", "H", "D", "E")
quadnum <- c(rep("NA", 4))


#this for loop generates how many quads per section 
for (i in 1:4) {
  
  quadnum[i] <- length(grep(sitelab[i], coor.d$Section))
  #sitelab.quad[i] <- c(sitelab[i], rep(NA, quadnum[i]))
}
#9, 9, 9, 3

#create an empty object 9 long (max number of quads)
exp <- rep("NA", 9)

#make a data frame four long (this is the only way i could get the format to 
  #output properly)
allquad <- data.frame(exp, exp, exp, exp)

#this for loop pastes each site label with each number from 1 - 9 and stores it
  #into the data frame object created above (each site as a column)
for (i in 1:4) {
  
  allquad[, i] <- paste(sitelab[i], seq(1, 9, 1), sep = "")
   
}

#tranform four column data frame into a dataframe with a single column
  #Additionally we know that site E (the 4th column) has only 3 quadrats
allist <- c(allquad[,1], allquad[,2], allquad[,3], allquad[1:3,4])
allist.d <- data.frame(allist)

#***I ideally want to make what i name it something more clear, but to do that
  #i need to change the name of the column in the big data frame and i still dk 
  #how to do that
colnames(allist.d) <- "X"

#merge the two data frames into a new data frame by the column "X" while keeping
  #the variables in the first dataframe
allquad <- merge(allist.d, inv.d, by = "X", all.x = TRUE)
allquad$X #the column now contains at least one row of every quad type

#We want to replace all the rows where number of individuals = NA to equal 0

#This for loop will run for every row in our data frame
for (i in 1:nrow(allquad)) {
  
  #The if statement says that if that row of num. of ind. = NA than make it 0
  if (is.na(allquad$Number.of.individuals[i])){
  
    allquad$Number.of.individuals[i] <- 0
  }

}

#If we look we see all the NAs have been replaced by 0 but the rows with values 
  #already are the same

#create path to save in clean file
p.allquad <- paste(p.final[2], "allquad.csv", sep = "")
write.csv(allquad, file = p.allquad)

