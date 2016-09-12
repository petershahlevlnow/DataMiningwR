#Chapter 4 detecting fraudulent transactions
# detecting whether salesman sales reports are "suspicious": outliers from the norm
library(DMwR)
data(sales)
head(sales) 

#ID - sales person ID
#Prod - product sold
#Quant - quantitiy sold
#Val- total monetary value
# Insp - levele - ok: good transactoin, unkn: not inspected, fraud: is detected fraud.

summary(sales)
nlevels(sales$ID)
nlevels(sales$Prod) # num of factor levels

# find how many obs. with Val and Quant are both NAs
length(which(is.na(sales$Val) & is.na(sales$Quant)))
sum(is.na(sales$Val) & is.na(sales$Quant))

# proportion of inspection levels
table(sales$Insp)/nrow(sales) * 100

# view barplot
totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS, main = "Transactions per sales person", names.arg = "", 
        xlab = "Salespeople", ylab = "Amount")
barplot(totP, main = "Transactions per product", names.arg = "", 
        xlab = "Product", ylab = "Amount")

# products show some variability so makes sense to find unit price for products
sales$Uprice <- sales$Val/sales$Quant
summary(sales$Uprice)

# marked variablity , so analyze for suspicious activity by product
# of the 4000+ products 900+ have less than 20 reports which is too few to make claims on
# find top and bottom and get median for all products
attach(sales) # attaches data so object names are only necessary
upp <- aggregate(Uprice, list(Prod), median, na.rm = T)
topP <- sapply(c(T,F), function(o) upp[order(upp[,2], decreasing = o)[1:5], 1])
colnames(topP) <- c('Expensive', 'Cheap')
topP # top expensive and cheap products

tops <- sales[Prod %in% topP[1, ], c("Prod", "Uprice")]
tops$Prod <- factor(tops$Prod)
boxplot(Uprice ~ Prod, data = tops, ylab = "Uprice", log = "y") # view on log scale

# can do the same analysis on salespeople (who brings in most/least money)
vs <- aggregate(Val, list(ID), sum, na.rm = T)
scoreSs <- sapply(c(T,F), function(o) vs[order(vs$x, decreasing = o)[1:5], 1])
colnames(scoreSs) <- c('Most', 'Least')
scoreSs

#top 100 accounts for nearly 40% of companies income
sum(vs[order(vs$x, decreasing = T)[1:100], 2])/sum(Val, na.rm = T) * 100
# bottom 2000 account for less than 2%
sum(sum(vs[order(vs$x, decreasing = F)[1:2000], 2])/sum(Val, na.rm = T) * 100)

# quantity of product shows imbalance
qs <- aggregate(Quant, list(Prod), sum, na.rm = T)
scoresPs <-  sapply(c(T,F), function(o) qs[order(qs$x, decreasing = o)[1:5], 1])
colnames(scoresPs) <- c("Most", "Least")
scoresPs

# top 100 products account for 75% of volume, and bottom 4000 < 9%
sum(as.double(qs[order(qs$x, decreasing = T)[1:100], 2]))/sum(as.double(Quant), na.rm = T) * 100
sum(as.double(qs[order(qs$x, decreasing = F)[1:4000], 2]))/sum(as.double(Quant), na.rm = T) * 100

# boxplot stats rules - outlier rules of quartiles, works for normally distribution
out<- tapply(Uprice, list(Prod = Prod), function(x) length(boxplot.stats(x)$out))
out[order(out, decreasing = T)[1:10]] # how many outliers per the top 10 products
sum(out) # outlier sum for all products
sum(out)/nrow(sales)*100 # proportion of outliers




