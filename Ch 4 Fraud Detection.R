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

# 4.3.2.3 Normalized Distance to Typical Price
# can use distance from typical unit price to determine outliers for unlabeled observations
# we need to normalize the distance unit to avoid running into differing scale issues
# use IQR to normalize distance
# NDTPp(u) = |u-Up| / IQRp, Up is typical unit price for product, IQRp is the interquartile
# range for unit prices for product

avgNTDP <- function(toInsp, train, stats){
  if(missing(train) && missing(stats))
    stop('provide either training data or product stats')
  if(missing(stats)){
    notF <- which(train$Insp != 'fraud')
    stats <- tapply(train$Uprice[notF],
                    list(Prod=train$Prod[notF]),
                    function(x) {
                      bp <- boxplot.stats(x)$stats
                      c(median=bp[3], iqr=bp[4] -bp[2])
                    })
    stats <- matrix(unlist(stats),
                    length(stats), 2, byrow = T,
                    dimnames = list(names(stats), c('median', 'iqr')))
    stats[which(stats[,'iqr']==0), 'iqr'] <- stats[which(stats[,'iqr']==0, 'median')]
  }
  mdtp <- mean(abs(toInsp$Uprice - stats[toInsp$Prod,'median'])/stats[toInsp$Prod,'iqr'])
  return(mdtp)
}

# the function above recieves the transactions for inspection. 
# if training data but not stats is supplied then function calculates the IQR values and median
#   of non-fraudulent transactions
# if stats is supplied then the function goes straight to calculation of NDTP
# IQR may be zero for cases with few transactions. to avoid division by zero we set these cases
# to the value of the median

# 4.3.3 Experimental Methodology
# Our transaction data can be split 70% to 30%, where 30% is the test set
# WE have an imbalance with cases that are labled to not. To avoid distributions issues
# use a stratified sampling. 
# need to repspect the original proportions of the data by pulling random samples from two
# different bags class x = 10% class y =90%, we want 100 cases. Observations are broken into 
# two sets X & Y. 10 are chosen from X and 90 are chosen from Y, randomly.
# the holdout() function can make this process easy.

# function below calculates evaluation statistics - recall, precision, NDTP of sample
evalOutlierRanking <- function(testSet, rankOrder, Threshold, statsProds){
  ordTS <- testSet[rankOrder, ]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud', nF), rep('ok', N-nF)), ordTS$Insp) # confusion matrix
  prec <- cm['fraud', 'fraud']/sum(cm['fraud',])
  rec <- cm['fraud', 'fraud']/sum(cm[,'fraud'])
  AVGntdp <- avgNTDP(ordTS[nF, ], stats = statsProds)
  return(c(Precision = prec, Recall = rec, avgNTDP = AVGntdp))
}
# args = test set, ranking proposed by model, threshold specifying an inspection limit (% or count)
# statistics (median and IQR) of products

# We could put all products together then analyze
# Or WE could obtain a ranking for inspection for each product then make a global list
# But we will put all products toghther, get a stratified random sample/hold out, this 
# test set will be given different modelling techniques that should return a ranking of these transactions
# according to their estimated probability of being frauds. Internally, the models may
# decide to anlyze the products individually or all together

# 4.4.1 Unsupervised Approaches
# The modified box plot rule

# find outliers for near normally distributed variables with boxplots
# function below takes a training and test set. It gets the median and IQR. Gets
# an Outlier Score and rank orders test set data

BPrule <- function(train, test){
  notF <- which(train$Insp != 'fraud')
  ms <- tapply(train$Uprice[notF], list(Prod=train$Prod[notF]), 
               function(x){
                 bp <- boxplot.stats(x)$stats
                 c(median=bp[3], iqr= bp[4] - bp[2])
               })
  ms <- matrix(unlist(ms), length(ms), 2, byrow = T,
               dimnames = list(names(ms), c('median', 'iqr')))
  ms[which(ms[,'iqr'] == 0), 'iqr'] <- ms[which(ms[,'iqr'] == 0), 'median']
  ORscore <- abs(test$Uprice- ms[test$Prod, 'median']) / ms[test$Prod, 'iqr']
  return(list(rankOrder = order(ORscore, decreasing = T), rankScore = ORscore))
}
# Side note: could group products on similiarity in function above. if product has a 
# distribution of unit prices that is significantly similar. if there is such 
# a product we could add its transactions and thus obtain the estimate of median
# IQR for the larger sample. This should be done in the tapply function using data saved to
# similarProducts.Rdata

# evaluate simple method using holdout strategy.

notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF], list(Prod=sales$Prod[notF]), 
                      function(x){
                        bp <- boxplot.stats(x)$stats
                        c(median=bp[3], iqr= bp[4] - bp[2])
                      })
globalStats <- matrix(unlist(globalStats), length(globalStats), 2, byrow = T,
                      dimnames = list(names(globalStats), c('median', 'iqr')))
globalStats[which(globalStats[,'iqr'] == 0), 'iqr'] <- ms[which(globalStats[,'iqr'] == 0), 'median']

# hold out function needs to call a routine to obtain and evaluate BPrule for each 
# iterationof the experimental process
# the following function, which will be called from the holdOut() routine, returns 
# eval statistics with an attached attribute (extra info) with the predicted and true vals

ho.BPrule <- function(form, train, test,...){
  res <- BPrule(train, test)
  structure(evalOutlierRanking(test, res$rankOrder,...),
            itInfo = list(preds=res$rankScore, trues=ifelse(test$Insp=='fraud', 1, 0)))
}

bp.res <- holdOut(learner('ho.BPrule',
                           pars = list(Threshold=0.1,
                                       statsProds=globalStats)),
                   dataset(Insp ~. ,sales),
                   hldSettings(3, 0.3, s =1234, str = T), # (repitions, size, seed, stratified)
                   itsInfo = TRUE)

summary(bp.res)
# notes on results only 55% (52% in book) of known frauds are included in the top 10% (threshold)
# of reports. low values of precision mean that this method is putting mostly unknwn and ok cases
# in top 10%. avgNTDP means that the difference between the unit price of these reports and
# the median price of these reports of the same product is around 1.29 (1.8 in book) times
# the value of the IQR of these prices. 

par(mfrow=c(1,2))
info <- attr(bp.res, 'itsInfo')
PTs.bp <- aperm(array(unlist(info), dim=c(length(info[[1]]), 2, 3)), c(1,3,2))
PRcurve(PTs.bp[ , , 1], PTs.bp[ , , 2], main = 'PR curve', avg ='vertical')
CRchart(PTs.bp[ , , 1], PTs.bp[ , , 2], main = 'Cumulative Recall curve', avg ='vertical')

# from the cumulative recall curve we can get 40% recall without much effort, but to get
# 80% recall we would need to inspect 25%-30% of reports

# 4.4.1.2 Local outlier factor
# obtain a outlier score for each case by estimating its degree of isolation with respect
# to its local neighborhood. based on density of observations
# two main concepts 1) core distance p, distance to kth nearest neighbor 2) reachability 
# distance between p1 and p2, max of core distance and dist between both cases 
# 3) local reachability inversely proportional to the avg reachability dist to its k neighbors

# function LOF in book package for this, but only takes numeric variables. need to make dummy
# variables from nominals or just evaluate prods seperately
# use second method as it's less computationally intense

ho.LOF <- function(form, train, test, k, ...){
  ntr <- nrow(train)
  all <- rbind(train, test)
  N <- nrow(all)
  ups <- split(all$Uprice, all$Prod) # divide the uint prices of this 
                                     # full dataset by product
  r <- list(length = ups)
  for(u in seq(along=ups)) # applies LOF to each these sets of prices and applies 
                           # the LOF method to obtain an outlier factor for each 
                           # of the factors
    r[[u]] <- if (NROW(ups[[u]]) > 3)
                  lofactor(ups[[u]], min(k, NROW(ups[[u]]) %/% 2))
              else if (NROW(ups[[u]])) rep(0, NROW(ups[[u]]))
              else NULL
  all$lof <- vector(length = N)
  split(all$lof, all$Prod) <- r # obtained outliers are attached to respective transactions
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
      SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))]) # chages outlier factor
                                                                         # into 0..1 scale
  structure(evalOutlierRanking(test, order(all[(ntr + 1):N, 'lof'], decreasing = T),...),
            itInfo = list(preds=all[(ntr + 1):N, 'lof'], trues=ifelse(test$Insp=='fraud', 1, 0)))
            # eval scores, predicted, and trues
}

# hold out strategy

lof.res <- holdOut(learner('ho.LOF',
                          pars = list(k=7, Threshold=0.1,
                                      statsProds=globalStats)),
                  dataset(Insp ~. ,sales),
                  hldSettings(3, 0.3, s =1234, str = T), # (repitions, size, seed, stratified)
                  itsInfo = TRUE)
summary(lof.res)

par(mfrow=c(1,2))
info <- attr(lof.res, 'itsInfo')
PTs.lof <- aperm(array(unlist(info), dim=c(length(info[[1]]), 2, 3)), c(1,3,2))
PRcurve(PTs.bp[ , , 1], PTs.bp[ , , 2], main = 'PR curve', lty = 1, xlim=c(0,1), 
        ylim = c(0,1), avg ='vertical')
PRcurve(PTs.lof[ , , 1], PTs.lof[ , , 2], lty = 2, add = T,  avg ='vertical')
legend('topright', c('BPrule', 'LOF'), lty = c(1,2))
CRchart(PTs.bp[ , , 1], PTs.bp[ , , 2], main = 'Cumulative Recall curve', lty = 1, xlim=c(0,1),
        ylim = c(0,1), avg ='vertical')
CRchart(PTs.lof[ , , 1], PTs.lof[ , , 2], add= T, lty = 2, avg ='vertical')
legend('bottomright', c('BPrule', 'LOF'), lty = c(1,2))


# 4.4.1.3 Clustering Based Oulier Rankings (ORh)
# clustering to obtain dendreon - clusterings of merging data in steps
# ie hierachical clustering
# the thought here is that outliers will avoid earlier merging because of dissimalarity 
# so they will be merged toghether last

ho.ORh <- function(form, train, test,...){
   ntr <- nrow(train)
   all <- rbind(train, test)
   N <- nrow(all)
   ups <- split(all$Uprice, all$Prod) # divide the uint prices of this 
  # full dataset by product
   r <- list(length = ups)
   for(u in seq(along=ups)) # applies LOF to each these sets of prices and applies 
     # the LOF method to obtain an outlier factor for each 
     # of the factors
    r[[u]] <- if (NROW(ups[[u]]) > 3)
                  outliers.ranking(ups[[u]])$prob.outliers
              else if (NROW(ups[[u]])) rep(0, NROW(ups[[u]]))
              else NULL
   all$orh <- vector(length = N)
   split(all$orh, all$Prod) <- r # obtained outliers are attached to respective transactions
   all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))] <- 
      SoftMax(all$orh[which(!(is.infinite(all$orh) | is.nan(all$orh)))]) # chages outlier factor
   # into 0..1 scale
   structure(evalOutlierRanking(test, order(all[(ntr + 1):N, 'orh'], decreasing = T),...),
            itInfo = list(preds=all[(ntr + 1):N, 'orh'], trues=ifelse(test$Insp=='fraud', 1, 0)))
    # eval scores, predicted, and trues
}

# this is too computationally intense, so use data file
orh.res <- holdOut(learner('ho.ORh',
                           pars = list(Threshold=0.1,
                                       statsProds=globalStats)),
                   dataset(Insp ~. ,sales),
                   hldSettings(3, 0.3, s =1234, str = T), # (repitions, size, seed, stratified)
                   itsInfo = TRUE)
load("Data/ORHresults.Rdata")
summary(orh.res)

par(mfrow=c(1,2))
info <- attr(orh.res, 'itsInfo')
PTs.orh <- aperm(array(unlist(info), dim=c(length(info[[1]]), 2, 3)), c(1,3,2))
PRcurve(PTs.bp[ , , 1], PTs.bp[ , , 2], main = 'PR curve', lty = 1, xlim=c(0,1), 
        ylim = c(0,1), avg ='vertical')
PRcurve(PTs.lof[ , , 1], PTs.lof[ , , 2], col='grey', add = T,  avg ='vertical')
PRcurve(PTs.orh[ , , 1], PTs.orh[ , , 2], col='red', add = T,  avg ='vertical')
legend('topright', c('BPrule', 'LOF','ORh'), lty= c(1,1,1), col = c('black','grey','red'))
CRchart(PTs.bp[ , , 1], PTs.bp[ , , 2], main = 'Cumulative Recall curve', lty = 1, xlim=c(0,1),
        ylim = c(0,1), avg ='vertical')
CRchart(PTs.lof[ , , 1], PTs.lof[ , , 2], add= T, col = 'grey', avg ='vertical')
CRchart(PTs.orh[ , , 1], PTs.orh[ , , 2], add= T, col = 'red', avg ='vertical')
legend('bottomright', c('BPrule', 'LOF', 'ORh'), lty = c(1,1,1), col = c('black','grey','red'))

# 4.4.2 Supervised Approaches 
# ranking reports based on probability of each class and target class

# 4.4.2.1 class imbalance - our dataset suffers from this. Only 8.1% of inspected reports are 
# fraud. Algorithms tend to ignore minority classes in prediction because they don't have statistical
# support, which is problematic when the minority class is your most relevant. 
# methods to overcome this:
# 1) methods that bias the learning process by using specific eval metrics that are more sensitive to minority class
# 2) sampling methods that monipulate the training data to change the class distribution
# we'll use the second in this example
# few ways to change the class distribution - 1) undersampling the majority and to the minority cases
# 2) over sampling works the other way around
# SMOTE - artificially generate new examples of the minority class using the nearest neighbor of these cases
# Furthermore, majority class is also under-sampled
# ex:
data(iris)
data <- iris[, c(1,2,5)]
data$Species <- factor(ifelse(data$Species == "setosa", "rare", "common"))
newData <- SMOTE(Species ~ ., data, perc.over = 600) # perc.over means x more cases for each of the original minority
table(newData$Species)

# visually see what happens
par(mfrow=c(1,2))
plot(data[,1], data[,2], pch = 19 + as.integer(data[,3]), main = "Original Data")
plot(newData[,1], newData[,2], pch = 19 + as.integer(newData[,3]), main = "SMOTE'd Data")

# 4.4.2.2 Naive Bayes
# probabalistic classifier
# Naive Bayes uses bayes theorem - P(A|B)= (P(B|A)*P(A)/P(B))
# calculates probabilty of each class for a given test case by 
# P(c|X1,...Xp) = P(c)*P(X1,...Xp|c)/P(X1,...,Xp)
# function below implements this and ranks the outlier on probability of the fraud class

nb <- function(train, test){
  require(e1071, quietly =T)
  sup <- which(train$Insp != "unkn")
  data <- train[sup, c("ID", "Prod", "Uprice", "Insp")]
  data$Insp <- factor(data$Insp, levels = c("ok", "fraud"))
  model <- naiveBayes(Insp ~ ., data)
  preds <- predict(model, test[, c("ID", "Prod", "Uprice", "Insp")], type = "raw")
  return(list(rankOrder = order(preds[,"fraud"], decreasing = T), rankScore = preds[, "fraud"]))
}

# hold out strategy function call
ho.nb <- function(form, train, test, ...){
  res <- nb(train, test)
  structure(evalOutlierRanking(test, res$rankOrder,...),
            itInfo = list(preds=res$rankScore, trues=ifelse(test$Insp=='fraud', 1, 0)))
}

# call holdout 
nb.res <-  holdOut(learner('ho.nb',
                           pars = list(Threshold=0.1,
                                       statsProds=globalStats)),
                   dataset(Insp ~. ,sales),
                   hldSettings(3, 0.3, s =1234, str = T), # (repitions, size, seed, stratified)
                   itsInfo = TRUE)
summary(nb.res)

# visually compare to ORh (unsupervised)
par(mfrow=c(1,2))
info <- attr(nb.res, 'itsInfo')
PTs.nb <- aperm(array(unlist(info), dim=c(length(info[[1]]), 2, 3)), c(1,3,2))
PRcurve(PTs.nb[ , , 1], PTs.nb[ , , 2], main = 'PR curve', lty = 1, xlim=c(0,1), 
        ylim = c(0,1), avg ='vertical')
PRcurve(PTs.orh[ , , 1], PTs.orh[ , , 2], col='red', add = T,  avg ='vertical')
legend('topright', c('Naive Bayes', 'ORh'), lty= c(1,1), col = c('black','red'))
CRchart(PTs.nb[ , , 1], PTs.nb[ , , 2], main = 'Cumulative Recall curve', lty = 1, xlim=c(0,1),
        ylim = c(0,1), avg ='vertical')
CRchart(PTs.orh[ , , 1], PTs.orh[ , , 2], add= T, col = 'red', avg ='vertical')
legend('bottomright', c('BPrule', 'LOF', 'ORh'), lty = c(1,1), col = c('black','red'))

# Poor perfomance relative to ORh maybe due to class imbalance. Use SMOTE with NB
nb.s <- function(train, test){
  require(e1071, quietly =T)
  sup <- which(train$Insp != "unkn")
  data <- train[sup, c("ID", "Prod", "Uprice", "Insp")]
  data$Insp <- factor(data$Insp, levels = c("ok", "fraud"))
  newData <- SMOTE(Insp ~ ., data, perc.over = 700) # using SMOTE to deal with class imbalance
  model <- naiveBayes(Insp ~ ., newData)
  preds <- predict(model, test[, c("ID", "Prod", "Uprice", "Insp")], type = "raw")
  return(list(rankOrder = order(preds[,"fraud"], decreasing = T), rankScore = preds[, "fraud"]))
}

# call holdout 
ho.nbs <- function(form, train, test, ...){
  res <- nb.s(train, test)
  structure(evalOutlierRanking(test, res$rankOrder,...),
            itInfo = list(preds=res$rankScore, trues=ifelse(test$Insp=='fraud', 1, 0)))
}

nbs.res <-  holdOut(learner('ho.nbs',
                           pars = list(Threshold=0.1,
                                       statsProds=globalStats)),
                   dataset(Insp ~. ,sales),
                   hldSettings(3, 0.3, s =1234, str = T), # (repitions, size, seed, stratified)
                   itsInfo = TRUE)

summary(nbs.res)

# visually compare to ORh (unsupervised) and nb
par(mfrow=c(1,2))
info <- attr(nbs.res, 'itsInfo')
PTs.nbs <- aperm(array(unlist(info), dim=c(length(info[[1]]), 2, 3)), c(1,3,2))
PRcurve(PTs.nbs[ , , 1], PTs.nbs[ , , 2], main = 'PR curve', lty = 1, xlim=c(0,1), 
        ylim = c(0,1), avg ='vertical')
PRcurve(PTs.nb[ , , 1], PTs.nb[ , , 2], col='grey', add = T,  avg ='vertical')
PRcurve(PTs.orh[ , , 1], PTs.orh[ , , 2], col='red', add = T,  avg ='vertical')
legend('topright', c('SMOTE NB', 'Naive Bayes', 'ORh'), lty= c(1,1,1), col = c('black','grey','red'))
CRchart(PTs.nbs[ , , 1], PTs.nbs[ , , 2], main = 'Cumulative Recall curve', lty = 1, xlim=c(0,1),
        ylim = c(0,1), avg ='vertical')
CRchart(PTs.nb[ , , 1], PTs.nb[ , , 2], add= T, col = 'grey', avg ='vertical')
CRchart(PTs.orh[ , , 1], PTs.orh[ , , 2], add= T, col = 'red', avg ='vertical')
legend('bottomright', c('SMOTE NB', 'Naive Bayes', 'ORh'), lty = c(1,1,1), col = c('black','grey', 'red'))

# the SMOTE and NB both underperform the ORh which may be due to not splitting the model based on
# product.

# 4.4.2.3 AdaBoost
# belongs to an ensemble models - base models that contribute to the prediction algorithm using
# some form of aggregation. 
# adaptive boosting method to obtain a set of baseline models. sequentially improving on errors
# improvements are made using weighting schema that increases the weights of the cases that are 
# incorrectly classified in previous model. predictions are based on the weighted average of the
# prediction of individual base models.
# using RWeka, can also use adabag::adboost.M1() but predict doesn't return class probabilities
# which is an issue.

library(RWeka)
WOW(AdaBoostM1)
data(iris)
idx <- sample(150, 100)
model <- AdaBoostM1(Species ~., iris[idx, ], control = Weka_control(I=100))
preds <- predict(model, iris[-idx,])


# using adabag since updates to adabag::predict.boosting return probabilites??
# Doesn't work...RWeka throws Java version errors. Can't figure out AdaBag....
# Code is below but doesn't work going to load results from website.
library(adabag)
data(iris)
idx <- sample(150, 100)
model <- boosting(Species ~., iris[idx, ], mfinal = 100)
preds <- predict.boosting(model, iris[-idx,])
head(preds)
head(preds$prob)
table(preds, iris[-idx, 'Species'])

ab <- function(train, test){
  require(adabag, quietly =T)
  sup <- which(train$Insp != "unkn")
  data <- train[sup, c("ID", "Prod", "Uprice", "Insp")]
  data$Insp <- factor(data$Insp, levels = c("ok", "fraud"))
  model <- boosting(Insp ~ ., data, mfinal = 1)
  preds <- predict.boosting(model, test[, c("ID", "Prod", "Uprice", "Insp")])
  return(preds)
  #return(list(rankOrder = order(preds$prob[,'fraud'], decreasing = T), rankScore = preds$prob[, 'fraud']))
}

ho.ab <- function(form, train, test, ...){
  res <- ab(train, test)
  #structure(evalOutlierRanking(test, res$rankOrder,...),
  #          itInfo = list(preds=res$rankScore, trues=ifelse(test$Insp=='fraud', 1, 0)))
}

ab.res <-  holdOut(learner('ho.ab',
                            pars = list(Threshold=0.1,
                                        statsProds=globalStats)),
                    dataset(Insp ~. ,sales),
                    hldSettings(3, 0.3, s =1234, str = T), # (repitions, size, seed, stratified)
                    itsInfo = TRUE)
###################################################
### AdaBoost with RWeka
###################################################
library(RWeka)
WOW(AdaBoostM1)


data(iris)
idx <- sample(150,100)
model <- AdaBoostM1(Species ~ .,iris[idx,],
                    control=Weka_control(I=100))
preds <- predict(model,iris[-idx,])
head(preds)
table(preds,iris[-idx,'Species'])
prob.preds <- predict(model,iris[-idx,],type='probability')
head(prob.preds)


ab <- function(train,test) {
  require(RWeka,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- AdaBoostM1(Insp ~ .,data,
                      control=Weka_control(I=100))
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}


ho.ab <- function(form, train, test, ...) {
  res <- ab(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}


ab.res <- holdOut(learner('ho.ab',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
)


summary(ab.res)

load("Data/ABresults.RData")

par(mfrow=c(1,2))
info <- attr(ab.res, 'itsInfo')
PTs.ab <- aperm(array(unlist(info), dim=c(length(info[[1]]), 2, 3)), c(1,3,2))
PRcurve(PTs.ab[ , , 1], PTs.ab[ , , 2], main = 'PR curve', lty = 1, xlim=c(0,1), 
        ylim = c(0,1), avg ='vertical')
PRcurve(PTs.nb[ , , 1], PTs.nb[ , , 2], col='grey', add = T,  avg ='vertical')
PRcurve(PTs.orh[ , , 1], PTs.orh[ , , 2], col='red', add = T,  avg ='vertical')
legend('topright', c('AdaBoost', 'Naive Bayes', 'ORh'), lty= c(1,1,1), col = c('black','grey','red'))
CRchart(PTs.ab[ , , 1], PTs.ab[ , , 2], main = 'Cumulative Recall curve', lty = 1, xlim=c(0,1),
        ylim = c(0,1), avg ='vertical')
CRchart(PTs.nb[ , , 1], PTs.nb[ , , 2], add= T, col = 'grey', avg ='vertical')
CRchart(PTs.orh[ , , 1], PTs.orh[ , , 2], add= T, col = 'red', avg ='vertical')
legend('bottomright', c('AdaBoost', 'Naive Bayes', 'ORh'), lty = c(1,1,1), col = c('black','grey', 'red'))

# cumalative recall is as good ORh, which is what we are looking for. 
# 4.4.3 Semi-Supervised - using inspected and non-inspected reports to obtain classification model
# Self training - building an initial classifier using the given labeled cases, then predict unlabeled
# higher confidence predictions are added to the initial model and iterated for classification.
# self training method needs 1) base learner 2) threshold on the confidence of classifications 3)
# criteria to decide when to terminate process

# example with iris data
library(DMwR)
library(e1071)
data(iris)
idx <- sample(150, 100) # get 100 labled samples, row index random 1...150
tr <- iris[idx, ] # train
ts <- iris[-idx, ] # test
nb <- naiveBayes(Species ~., tr) # first naive bayes
table(predict(nb, ts), ts$Species)

trST <- tr
nas <- sample(100, 90) # get 90 samples and label them as NA
trST[nas, "Species"] <- NA 
func <- function(m,d) { # must make this function if using self train
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max)) # func returns a data frame with
  # first column - predicted cases with labels, second column - respective probability of that classification
}

nbSTbase <- naiveBayes(Species ~., trST[-nas, ]) # 2nd nb for the base model with remaining 10 samples
table(predict(nbSTbase, ts), ts$Species)
nbST <- SelfTrain(Species ~., trST, learner("naiveBayes", list()), "func") # self train with labeled and unlabeled cases
table(predict(nbST, ts), ts$Species)

# other notes on self train - parameters - thrConf sets probability required for an unlabeled to be 
# merged into a labeled set. Parameter maxits - iterations, percFull - when the process should stop.
# unlabeled cases must be signaled with NA

pred.nb <- function(m,d) {
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)], p=apply(p,1,max))
}
nb.st <- function(train,test) {
  require(e1071,quietly=T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(Insp ~ .,train, learner('naiveBayes', list()),'pred.nb')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')], type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T), rankScore=preds[,'fraud']))
}
ho.nb.st <- function(form, train, test, ...) {
  res <- nb.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore, trues=ifelse(test$Insp=='fraud',1,0)))
}

nb.st.res <- holdOut(learner('ho.nb.st',
                             pars=list(Threshold=0.1,
                                       statsProds=globalStats)),
                     dataset(Insp ~ .,sales),
                     hldSettings(3,0.3,1234,T),
                     itsInfo=TRUE
)


summary(nb.st.res)


par(mfrow=c(1,2))
info <- attr(nb.st.res,'itsInfo')
PTs.nb.st <- aperm(array(unlist(info), dim=c(length(info[[1]]),2,3)), c(1,3,2))
PRcurve(PTs.nb[,,1],PTs.nb[,,2], main='PR curve',lty=1, xlim=c(0,1), ylim=c(0,1), avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2], add=T,lty=1,col='grey', avg='vertical')        
PRcurve(PTs.nb.st[,,1],PTs.nb.st[,,2], add=T,lty=2, avg='vertical')        
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'), lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.nb[,,1],PTs.nb[,,2], main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1), avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2], add=T,lty=1,col='grey', avg='vertical')        
CRchart(PTs.nb.st[,,1],PTs.nb.st[,,2], add=T,lty=2, avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'), lty=c(1,1,2),col=c('black','grey','black'))


pred.ada <- function(m,d) { 
  p <- predict(m,d,type='probability')
  data.frame(cl=colnames(p)[apply(p,1,which.max)], p=apply(p,1,max))
} 
ab.st <- function(train,test) {
  require(RWeka,quietly=T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(Insp ~ .,train,
                     learner('AdaBoostM1',
                             list(control=Weka_control(I=100))),
                     'pred.ada')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}
ho.ab.st <- function(form, train, test, ...) {
  res <- ab.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}
ab.st.res <- holdOut(learner('ho.ab.st',
                             pars=list(Threshold=0.1,
                                       statsProds=globalStats)),
                     dataset(Insp ~ .,sales),
                     hldSettings(3,0.3,1234,T),
                     itsInfo=TRUE
)


summary(ab.st.res)

load("Data/ABSTresults.Rdata")

par(mfrow=c(1,2))
info <- attr(ab.st.res,'itsInfo')
PTs.ab.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                   c(1,3,2)
)
PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.ab.st[,,1],PTs.ab.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('AdaBoostM1','ORh','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.ab[,,1],PTs.ab[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.ab.st[,,1],PTs.ab.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('AdaBoostM1','ORh','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))


