# Chapter 5 Classifcation of Microarray Samples
# try to predict disease states using gene levels (variables) from observations/samples (individuals)

# 5.2 Get Data and packages from Biaconductor website (need to only do this once)
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("ALL")

# use the data set, 
library(Biobase)
library(ALL)
data("ALL") # gets a special class from Biobase
ALL

# get specific cases of BT and mio.biol factors
tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & 
                     ALL$mol.biol %in% levels(ALL$mol.biol)[1:4])
ALLb <- ALL[,tgt.cases] #data set is trasposed (observations are columns, and variables are rows)
ALLb

ALLb$BT <- factor(ALLb$BT)
ALLb$mol.biol <- factor(ALLb$mol.biol)
save(ALLb, file = "Data/myALL.Rdata")

