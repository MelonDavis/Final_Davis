#Clean

#====== [Load Clean] =====

#Data was cleaned in excel to match coordinate data (distance from high tide)
  #and results of each quadrat

#import data. NAs were entered as "-" so include transforming them into NA
inv.d <- read.csv(file.choose("invertebrate"), stringsAsFactors = FALSE, 
                  strip.white = TRUE, na.strings = c("NA","-"))

tail(inv.d) #successfully transformed dashes to NA

#We can see that data has only been entered for quadrats in which there were 
  #species, however if we want to represent all the data gathered we need to 
  #include the result of 0 for those quadrats w/ no species found 

#import coordinate data for every quadrat
coor.d <- 

nrow(inv.d) #118
 #19


inv.d[118 + i,] <- rbind(rep(NA, ncol(inv.d)))
tail(inv.d)
