#Clean

#====== [Load Raw] =====

inv.d <- read.csv(file.choose("invertebrate"), stringsAsFactors = FALSE, strip.white = TRUE, 
                  na.strings = c("NA",""))

head(inv.d)

#In this case we want to keep site section, quadrat, kingdom, phylum, class, 
  #order