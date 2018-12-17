#Main

#===== [SYSTEM] =======

R.version.string
#"R version 3.5.1 (2018 - 07 - 02)"

#create object of working directory
working.dir <- getwd()

#download packages
#**download ggplot?

#===== [FILE MANAGEMENT] =====

#stored names of folders into an object
output.folder.names <- c("raw.data", "clean.data", "analysis")

#for loop combo with if function that asks whether the each position of 
#output.folder.names exists as a file and if it's true that it doesn't exist
#it will create it.
for(i in 1:length(output.folder.names)) {
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])
}

#Make path names for each folder

#concenate empty object to store paths
p.final <- c(rep(NA, 3)) 

#for loop that generates path for each folder and assigns path to corresponding
#position in paths.proto
for (i in 1:length(output.folder.names)) {
  p.final[i] <- paste(working.dir, "/", output.folder.names[i], "/", 
                            sep = "")
  
} 
#paths.bigdata should now contain each pathway stored in the same order as 
#output.folder.names.

print(p.final)
#1 = raw; 2 = clean; 3 = analysis


#=====[FUNCTIONS] ======

f.matchtest <- function(setone, settwo){
  
  listone <- c(rep(NA, 51)) #state names from pop data
  listtwo <- c(rep(NA, 51)) #state names from size data
  matchtest <- c(rep("NA", 51))
  
  for(i in 1:51) {
    
    listone[i] <- c(substr(setone[i], 1, 50))
    listtwo[i] <- c(substr(settwo[i], 1, 50))
    
    matchtest[i] <- listone[i] == listtwo[i]
    
  }
  print(matchtest)
}




