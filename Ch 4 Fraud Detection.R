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

# 4.2.3.1 data quality problems
# unknown values

# 3 ways to handle unknown variable values: 1) remove 2) impute 3) strategy to handle as is
totS <- table(ID)
totP <- table(Prod)
nas <- sales[which(is.na(Quant) & is.na(Val)), c("ID", "Prod")] #salespeople involved with problematic transactions
# find the proportion of of transactions that a salesperson has thats unknown
propS <-100*table(nas$ID)/totS
propS[order(propS, decreasing = T)[1:10]] # top 10 proportions

# find the proportion of unknowns for product
propP <- 100*table(nas$Prod)/totP
propP[order(propP, decreasing = T)[1:10]] 

# only resonable option is to remove unknowns, see page 176 book for more details
detach(sales) # if using attach need to use detach if you are getting rid of observations
sales <- sales [-which(is.na(sales$Val) & is.na(sales$Quant)),]
# examine quantity unknowns for products
nnasQp <- tapply(sales$Quant, list(sales$Prod), function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp, decreasing = T)[1:10]]
# two products have all of their transactions with unknown quantities
# delete 
sales <- sales[!sales$Prod %in% c("p2442", "p2443"),]
nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)

# Are there sales people with all transactions with unknown Quantity?
nnasQs <- tapply(sales$Quant, list(sales$ID), function(x) sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs, decreasing = T)[1:10]] # it is ok to keep these since we can impute other sales people's unit price into here

# unknown Vals?
nnasVp <- tapply(sales$Val, list(sales$Prod), function(x) sum(is.na(x)))
propNAsVp <- nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp, decreasing = T)[1:10]]

nnasVs <- tapply(sales$Val, list(sales$ID), function(x) sum(is.na(x)))
propNAsVs <- nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs, decreasing = T)[1:10]]

# impute the median unit price for products on transactions 
tPrice <- tapply(sales[sales$Insp != "fraud", "Uprice"], 
                 list(sales[sales$Insp != "fraud", "Prod"]), median, na.rm = T)
# with unit prices and at least Quant or Val we can calculate the other
# fill in all remaining unknowns with values
noQuant <- which(is.na(sales$Quant))
sales[noQuant, 'Quant'] <- ceiling(sales[noQuant, 'Val']/tPrice[sales[noQuant, 'Prod']])
noVal <- which(is.na(sales$Val))
sales[noVal, 'Val'] <- sales['noVal', 'Quant'] * tPrice[sales[noVal, 'Prod']]

sales$Uprice <- sales$Val/sales$Quant

save(sales, file = "Data/salesClean.Rdata")

# 
attach(sales)










