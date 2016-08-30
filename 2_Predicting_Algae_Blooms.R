# Gettting data
# Using library instead web call, as the site is down. piece of shit
library(DMwR)
library(car)
head(algae)
nrow(algae)
algae1 <- algae

# replace NA with 'XXXXX'
# algae[is.na(algae)] <- c('XXXXX')

# change column 
#summary statistics
summary(algae)

#max pH density function and Quartile Quartile plot.
hist(algae1$mxPH, prob =T)
par(mfrow= c(1,1))   #<- Change to (1,2) for side by side graphs
hist(algae1$mxPH, prob = T, xlab= '', main = 'Histogram of Max PH value', ylim = 0:1)
lines(density(algae1$mxPH, na.rm = T))
rug(jitter(algae1$mxPH))
qqPlot(algae1$mxPH, main = 'QQ plot mxPh')

#boxplot oP04
boxplot(algae1$oPO4, ylab = 'Orthopohsphate (oPO4)')
rug(jitter(algae1$oPO4), side = 2)
abline(h = mean(algae1$oPO4, na.rm = TRUE), lty = 2)

# plot for NH4
plot(algae1$NH4, xlab = '')
abline(h = mean(algae1$NH4, na.rm = T), lty = 1)
abline(h = mean(algae1$NH4, na.rm = T) + sd(algae1$NH4), lty = 2)
abline(h = median(algae1$NH4, na.rm = T), lty = 3)
clicked.lines <-identify(algae1$NH4)
algae1[clicked.lines,]
algae1[!is.na(algae1$NH4) & algae$NH4 > 19000,]

#Size on Alagal a1
library(lattice)
bwplot(size ~ a1, data = algae1, ylab = "size", xlab = "Algal a1")

#violin plot of sorts
library(Hmisc)
bwplot(size ~ a1, data = algae1, panel = panel.bpplot, probs=seq(0.01, 0.49, by = 0.1,), 
       datadensity = TRUE, ylab = 'River Size', xlab = 'Algal Al')

#discritize mnO2 into four bins with overlaps
minO2 <- equal.count(na.omit(algae1$mnO2), number = 4, overlap = 1/5)
minO2

stripplot(season ~ a1|minO2, data = algae1[!is.na(algae1$mnO2),])
