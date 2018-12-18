#Creating Linear Regression and scatter plots for our 

wd <- getwd()


#load data and replace dashes with "NA"
Invert <- read.csv("invertebrate_raw_new.csv", na.strings = c("NA", "-"))
View(Invert)

#---TOTAL (without B.glandula)-------------------------------------------------

Inv.Phy <- unique(Invert$Phylum)
col.inv <- c("springgreen", "steelblue1", "tomato", "purple")

xlim.inv <- c(0, 200)
ylim.inv <- c(0, 200)
plot(NA, xlim = xlim.inv, ylim = ylim.inv, xlab = "Distance from HTL (ft)", 
     ylab = "Abundance", main = "Invertebrates (Without B.glandula)")

for(i in 1:length(Inv.Phy)){
  d.inv <- Invert[Invert$Phylum == Inv.Phy[i] & 
                    Invert$Number.of.individuals < 177, ]
  points(d.inv$WE..ft.from.HTL., d.inv$Number.of.individuals, col=col.inv[i],
         pch = 19)
  
}
#Creating regression lines for each order

inv.interc.t <- inv.slope.t <- rep(NA,length(Inv.Phy))


for(i in 1:length(Inv.Phy)){
  #i <- 6
  # select first Order
  d.inv2 <- Invert[Invert$Phylum == Inv.Phy[i] & 
                     Invert$Number.of.individuals < 177, ]
  m.inv <- lm(d.inv2$Number.of.individuals ~ d.inv2$WE..ft.from.HTL.)
  inv.slope.t[i] <- m.inv$coefficients[2]
  inv.interc.t[i] <- m.inv$coefficients[1]
  summary(m.inv)
  #adds lines
  abline(a=inv.interc.t[i], b=inv.slope.t[i], col = col.inv[i])
  
}


#Add a legend (Be sure it doesn't cover up points)
legend(110, 180, Inv.Phy, pch = 19, col = col.inv)

#---MOLLUSCA-------------------------------------------------------------------


Mollusca <- Invert[Invert$Phylum == "Mollusca" &
                     Invert$Number.of.individuals < 180, ]
str(Mollusca)
#View(Mollusca)
Mol.Ord <- unique(Mollusca$Order)
col.mol <- c("springgreen", "steelblue1", "tomato", "yellow2", "purple", 
             "navy", "dodgerblue")

#Create a plot for only the Phyllum Mollusca, colors are by order

xlim.mol <- range(Mollusca$WE..ft.from.HTL.)
ylim.mol <- range(Mollusca$Number.of.individuals)
plot(NA, xlim = xlim.mol, ylim = ylim.mol, xlab = "Distance from HTL (ft)", 
     ylab = "Abundance", main = "Mollusca")

for(i in 1:length(Mol.Ord)){
  d.mol <- Mollusca[Mollusca$Order == Mol.Ord[i], ]
  points(d.mol$WE..ft.from.HTL., d.mol$Number.of.individuals, col=col.mol[i],
         pch = 19)
  
}
#Creating regression lines for each order

mol.interc.t <- mol.slope.t <- rep(NA,length(Mol.Ord))


for(i in 1:(length(Mol.Ord) - 1)){
  i <- 6
  # select first Order
  d.mol2 <- Mollusca[Mollusca$Order == Mol.Ord[i],]
  m.mol <- lm(d.mol2$Number.of.individuals ~ d.mol2$WE..ft.from.HTL.)
  mol.slope.t[i] <- m.mol$coefficients[2]
  mol.interc.t[i] <- m.mol$coefficients[1]
  summary(m.mol)
  #adds lines
  abline(a=mol.interc.t[i], b=mol.slope.t[i], col = col.mol[i])
  
}

#Add a legend (Be sure it doesn't cover up points)
legend(130, 150, Mol.Ord, pch = 19, col = col.mol)

#---ARTHROPODA (withour B.glandula)--------------------------------------------
Arthropoda.wbg <- Invert[Invert$Phylum == "Arthropoda" & 
                           Invert$Number.of.individuals < 50, ]
str(Arthropoda.wbg)
#View(Arthropoda.wbg)
Ord.wbg <- unique(Arthropoda.wbg$Order)
col.arthro <- c("springgreen", "steelblue1", "tomato")

#Create a plot for only the Phyllum Arthropoda, colors are by order

# make empty plot
xlim.Arthro.wbg <- range(Arthropoda.wbg$WE..ft.from.HTL.)
ylim.Arthro.wbg <- range(Arthropoda.wbg$Number.of.individuals)
plot(NA, xlim = xlim.Arthro.wbg, ylim = ylim.Arthro.wbg, 
     xlab = "Distance from HTL (ft)", ylab = "Abundance",
     main = "Arthropoda (without B.glandula)")
for(i in 1:3){
  #i <- 1
  d.arthro.wbg <- Arthropoda.wbg[Arthropoda.wbg$Order == Ord.wbg[i], ]
  points(d.arthro.wbg$WE..ft.from.HTL., d.arthro.wbg$Number.of.individuals,
         col=col.arthro[i], pch = 19)
  
}

#Creating regression lines for each order

arthro.interc.wbg <- arthro.slope.wbg <- rep(NA,length(Ord.wbg))

for(i in 1:length(Ord.wbg)){
  #i <- 3
  # select first Order
  d.arthro.wbg2 <- Arthropoda.wbg[Arthropoda.wbg$Order == Ord.wbg[i],]
  m.arthro.wbg <- lm(d.arthro.wbg2$Number.of.individuals ~ 
                       d.arthro.wbg2$WE..ft.from.HTL.)
  arthro.slope.wbg[i] <- m.arthro.wbg$coefficients[2]
  arthro.interc.wbg[i] <- m.arthro.wbg$coefficients[1]
  #adds lines
  abline(a=arthro.interc.wbg[i], b=arthro.slope.wbg[i], col = col.arthro[i])
}

#Add a legend (Be sure it doesn't cover up points)
legend(120, 25, Ord, pch = 19, col = col.arthro)

#---ARTHROPODA (TOTAL)---------------------------------------------------------
Arthropoda <- Invert[Invert$Phylum == "Arthropoda", ]
str(Arthropoda)
#View(Arthropoda)
Ord <- unique(Arthropoda$Order)
col.arthro <- c("springgreen", "steelblue1", "tomato")

#Create a plot for only the Phyllum Arthropoda, colors are by order

# make empty plot
xlim.Arthro <- range(Arthropoda$WE..ft.from.HTL.)
ylim.Arthro <- range(Arthropoda$Number.of.individuals)
plot(NA, xlim = xlim.Arthro, ylim = ylim.Arthro, 
     xlab = "Distance from HTL (ft)", ylab = "Abundance", main = "Arthropoda")
for(i in 1:3){
  #i <- 1
  d.arthro <- Arthropoda[Arthropoda$Order == Ord[i], ]
  points(d.arthro$WE..ft.from.HTL., d.arthro$Number.of.individuals, 
         col=col.arthro[i], pch = 19)
  
}

#Creating regression lines for each order

arthro.interc.t <- arthro.slope.t <- rep(NA,length(Ord))

for(i in 1:length(Ord)){
  #i <- 3
  # select first Order
  d.arthro2 <- Arthropoda[Arthropoda$Order == Ord[i],]
  m.arthro <- lm(d.arthro2$Number.of.individuals ~ d.arthro2$WE..ft.from.HTL.)
  arthro.slope.t[i] <- m.arthro$coefficients[2]
  arthro.interc.t[i] <- m.arthro$coefficients[1]
  #adds lines
  abline(a=arthro.interc.t[i], b=arthro.slope.t[i], col = col.arthro[i])
}

#Add a legend (Be sure it doesn't cover up points)
legend(120, 1500, Ord, pch = 19, col = col.arthro)



#---ANNELIDA-------------------------------------------------------------------
Annelida <- Invert[Invert$Phylum == "Annelida", ]
str(Annelida)
#View(Annelida)
An.Ord <- unique(Annelida$Order)
col.an <- c("springgreen", "steelblue1", "tomato", "yellow2", "purple", "navy",
            "dodgerblue")
#Create a plot for only the Phyllum Annelida, colors are by order
xlim.an <- range(Annelida$WE..ft.from.HTL.)
ylim.an <- range(Annelida$Number.of.individuals)
plot(NA, xlim = xlim.an, ylim = ylim.an, xlab = "Distance from HTL (ft)", 
     ylab = "Abundance", main = "Annelida")

for(i in 1:length(An.Ord)){
  d.an <- Annelida[Annelida$Order == An.Ord[i], ]
  points(d.an$WE..ft.from.HTL., d.an$Number.of.individuals, col=col.an[i], 
         pch = 19)
  
}


#Creating regression lines for each order

an.interc.t <- an.slope.t <- rep(NA,length(An.Ord))

for(i in 1:length(An.Ord)){
  #i <- 2
  #select first Order
  d.an2 <- Annelida[Annelida$Order == An.Ord[i], ]
  m.an <- lm(d.an2$Number.of.individuals ~ d.an2$WE..ft.from.HTL.)
  an.slope.t[i] <- m.an$coefficients[2]
  an.interc.t[i] <- m.an$coefficients[1]
  summary(m.an)
  #adds lines
  abline(a=an.interc.t[i], b=an.slope.t[i], col = col.an[i])
  
}

#Add a legend (Be sure it doesn't cover up points)
legend(140, 3.5, An.Ord, pch = 19, col = col.an)

#---ECHINODERMATA--------------------------------------------------------------
Echinodermata <- Invert[Invert$Phylum == "Echinodermata", ]
str(Echinodermata)
#View(Echinodermata)
Ech.Ord <- unique(Echinodermata$Order)
col.ech <- c("limegreen")

#Create a plot for only the Phyllum Echinodermata, colors are by order

# make empty plot
xlim.Ech <- range(Echinodermata$WE..ft.from.HTL.)
ylim.Ech <- range(Echinodermata$Number.of.individuals)
plot(NA, xlim = xlim.Ech, ylim = ylim.Ech, 
     xlab = "Distance from HTL (ft)", ylab = "Abundance", 
     main = "Echinodermata")
for(i in 1:length(Ech.Ord)){
  #i <- 1
  d.ech <- Echinodermata[Echinodermata$Order == Ech.Ord[i], ]
  points(d.ech$WE..ft.from.HTL., d.ech$Number.of.individuals, col=col.ech[i], 
         pch = 19)
  
}

#Creating regression lines for each order

ech.reg <- lm(Echinodermata$Number.of.individuals ~ 
                Echinodermata$WE..ft.from.HTL.)
abline(ech.reg, col = "limegreen")

#Add a legend (Be sure it doesn't cover up points)
legend(130, 10, Ech.Ord, pch = 19, col = Ech.Ord)
