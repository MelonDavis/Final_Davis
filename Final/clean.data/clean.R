#Clean

#====== [Load Clean] =====

#Data was cleaned in excel to match coordinate data (distance from high tide)
  #and results of each quadrat

#import data. NAs were entered as "-" so include transforming them into NA
inv.d <- read.csv(file.choose("invertebrate"), stringsAsFactors = FALSE, 
                  strip.white = TRUE, na.strings = c("NA","-"))

tail(inv.d) #successfully transformed dashes to NA



