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

# 4.2.3.2 Few transactions of Some Products
# Need to deal with products that have too few transactions.
# possible to merge products with similar unit prices together
# can assume the distribution of product unit prices are normally distributed, but will 
# use the median as the unit of centrality due high number of outliers
attach(sales)
notF <- which(Insp != "fraud") #find rows not fraud
ms <- tapply(Uprice[notF], list(Prod=Prod[notF]), function(x) {
  bp <- boxplot.stats(x)$stats
  c(median = bp[3], iqr = bp[4] - bp[2]) 
}) # get boxplot stats of uprices on products
ms <- matrix(unlist(ms), length(ms), 2, byrow = T, dimnames = list(names(ms), 
                                                                   c('median', 'iqr')))
head(ms)
# plots 
par(mfrow = c(1,2))
plot(ms[,1], ms[,2], xlab = "Median", ylab = "IQR", main = "")
plot(ms[,1], ms[,2], xlab = "Median", ylab = "IQR", main = "", col = "grey", log = "xy")
smalls <- which(table(Prod) < 20)
points(log(ms[smalls, 1]), log(ms[smalls,2]), pch = "+")

# visual inspection is ok, but need a non-parametric test to test the assumption that
# the unit prices for the products come from same distribution. Kolmogorov-Smirnov test

dms <- scale(ms) # normalize 
smalls <- which(table(Prod) < 20) # get products with less than 20 transactions
prods <- tapply(sales$Uprice, sales$Prod, list)
similar <- matrix(NA, length(smalls), 7, dimnames = list(names(smalls), 
                                                         c("Simil", "ks.stat", "ks.p",
                                                           "medP", "iqrP", "medS", "iqrS")))
for(i in seq(along = smalls)){
  d <- scale(dms, dms[smalls[i], ], FALSE) # centering scale defined by second arg.
  d <- sqrt(drop(d^2 %*% rep(1, ncol(d)))) # finds the distribution distances for product "i"
  # second smallest distance is the closest to product i thus order(d)[2]
  stat <- ks.test(prods[[smalls[i]]], prods[[order(d)[2]]]) # ks test.
  similar[i, ] <- c(order(d)[2], stat$statistic, stat$p.value, ms[smalls[i], ], ms[order(d)[2], ])
}
head(similar)

levels(Prod)[similar[1, 1]]
# find the number of products that have a unit price distribution statistically similar
# with 90% confidence
sum(similar[, "ks.p"] >= .90)
save(similar, file = "Data/similarProducts.Rdata")


# 4.3 Defining the data mining tasks - unsupervised and supervised
# we'll be ranking the fraud probabilty

# Unsupervised learning -> use when you only have a set of independent variables (i.e descriptive data mining)
#   Clustering - grouping observations with similars by distance is one method 
#   Outlier detection - not part of any group. methods we use should detect and rank outliers
# Supervised portion - we'll use the small portion of marked observations to make predictions

# Classification problem since we are trying to predict a nominal variable (fraud or OK)
# a challenge is that less frequent identified observations fraud need to appropriate methods
# need a probablistic ranking for the classification - probablistic classification

# Semi-supervised - methods that can handle both labled and non-labled data. Typically
# with a smaller proportion that is labled.
# self-training is an approach of using labled data to build a model then using the model
# to predict unlabled data. The high confidence unlabled/predicted obserbations are added 
# to the original labled data and the process is repeated until some convergence metric is 
# hit.

#4.3.2.1 Evaluation criteria - recall and precision
# we want the known fraud cases to be ranked in the top posistions of our method
# precision tells us what proportion of the top cases are labeled fraud
# recall  what proportion of frauds are in the test are included in the k-top most 

# simple example from librarry ROCR to get plots for precisiona and recall

library(ROCR)
data("ROCR.simple")
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, "prec", "rec")
plot(perf)

# remove saw tooth from PR curve
PRcurve <- function(preds, trues, ...){
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "prec", "rec")
  pf@y.values <- lapply(pf@y.values, function(x) rev(cummax(rev(x)))) # cumulative max, and reverse vector
  plot(pf, ...)
}

PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

# confusion matrix example

#         Predictions
#         ok      Fraud               total
# True ok 3       1                   4
#         2       1                   3 (precision = 1 /(1+2))
#   total 5       2(recall = 1/(1+1)) 7    
#
# lift charts give more importance to recall thus fit our objective better
# x = rate of positive predictions y = recall

pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
pref <- performance(pred, "lift", "rpp")
plot(perf, main = "Lift Chart")

CRchart <- function(preds, trues, ...){
  require(ROCR, quietly = T)
  pd <- prediction(preds, trues)
  pf <- performance(pd, "rec", "rpp")
  plot(pf, ...)
}

CRchart(ROCR.simple$predictions, ROCR.simple$labels, main = 'Cumulative Recall')
