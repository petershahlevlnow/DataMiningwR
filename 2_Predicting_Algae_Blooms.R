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

#2.4 boxplot oP04
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

#2.5.1 Removing NAs and missing data
algae1[!complete.cases(algae1),]
nrow(algae1[!complete.cases(algae1),])

algaeNoNAs <- na.omit(algae1) #omit
algae62_199 <- algae1[-c(62,199),] #remove worst offenders explicitly

apply(algae1, 1, function(x) sum(is.na(x))) # 1 arg means first dimension in object

manyNAs(algae1, 0.2) # function in DMwR counts rows with more than 20% of data NA

algae62_199 <- algae1[-manyNAs(algae1, 0.2),]

#2.5.2 fitting unknowns with a statistic - mean, median, mode
# when using the mean for fittingensure distribution is normal
# median is better for skewed data

algae1[48, "mxPH"] <- mean(algae$mxPH, na.rm = T) # don't execute just an example
algae1[is.na(algae1$Chla, "Chla")] <- median(algae1$Chla, na.rm = T) # don't execute just an example.

# can also use centralImputation function part of the DMwR library
algae1 <- algae1[-manyNAs(algae1, 0.2),]
algae1 <- centralImputation(algae1)

# 2.5.3 using regression imputation
# look at correlation table

algCor <- cor(algae1[, 4:18], use = "complete.obs")
symnum(algCor)

algae1[is.na(algae1$PO4),] # row 28 only P04 obs with NA

#use linear regression to fill in missing values for PO4
lm(PO4 ~ oPO4, algae1)

#sapply function to all missing PO4 data

fillPO4 <- function(oP){
  if(is.na(oP))
    return(NA)
  else
    return (42.897 + oP * 1.293)
}
algae1[is.na(algae1$PO4), "PO4"] <- sapply(algae1[is.na(algae1$PO4), "oPO4"], fillPO4)

algae1$season <- factor(algae1$season, c("spring", "summer", "autumn", "winter"))
histogram(~mxPH|season, data = algae1)
algae1$season <- factor(algae1$season, c("spring", "summer", "autumn", "winter"))
histogram(~mxPH|season * speed, data = algae1)
stripplot(size ~ mxPH|speed, data = algae1, jitter = T)
