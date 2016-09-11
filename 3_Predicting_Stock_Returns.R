# Chapter 3 - Predicting Stock Market Returns
library(DMwR)
data("GSPC")
head(GSPC)
# multivariate time series where order matters
# illustrating a time series
library(xts)
x1 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01"), len = 100, by = "day"))
x1[1:5]
x2 <- xts(rnorm(100), seq(as.POSIXct("2000-01-01 13:00:00"), len = 100, by = "min"))
x2[1:5]
x3 <- xts(rnorm(3), as.Date(c("2005-01-01", "2005-01-10", "2005-01-12")))
x3

# subsetting based on date
x1[as.POSIXct("2000-01-04")]
x1["2000-01-05"]
x1["20000105"]
x1["2000-04"]
x1["2000-03-27/"] # dates greater than
x1["2000-02-26/2000-03-03"] # dates between
x1["/20000103"] # dates less than

# multiple time series
mts.val <- matrix(round(rnorm(25), 2), 5, 5)
colnames(mts.val) <- paste("ts", 1:5, sep = '')
mts <- xts(mts.val, as.POSIXct(c('2003-01-01', 
                                 '2003-01-04', 
                                 '2003-01-05', 
                                 '2003-01-12',
                                 '2003-01-16')))
mts
mts["2003-01",c("ts2","ts5")]
# index and time to extract the time tags info from xts obj. 
index(mts)
coredata(mts) # obtains the data values of time series

# 3.2.3 getting data from web (i.e Yahoo Finance) using get.hist.quote()
library(tseries)
GSPC <- as.xts(get.hist.quote("^GSPC", start = "1970-01-02", end = '2009-09-15',
                              quote = c("Open", "High", "Low", "Close", "Volume", "AdjClose")))

# another method is quantmod.getSymbols()
library(quantmod)
getSymbols("^GSPC")
getSymbols("^GSPC", from = "1970-01-01", to = "2009-09-15")
colnames(GSPC) <- c("Open", "High", "Low", "Close", "Volume", "AdjClose")

#getting multiple symbols from different data sources
# set symbol lookup allows to specify sources and settings & saveSymbolLookup() loadSymbolLookup
# saves and loads settings between R sessions 
setSymbolLookup(IBM=list(name= 'IBM', src = 'yahoo'), USDEUR = list(name = 'USD/EUR', src = 'oanda'))
getSymbols(c('IBM', "USDEUR"))
head(IBM)
head(USDEUR)

#3.3 prediction task - determine profitable times to buy and sell in order to place orders
# if prices vary more than p% then there is a worthwhile trade within k days
# we want an overall tendecy of prices OVER the next k days, not just where's the price after k days
# implement a simple indicator function. 

T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10){
  v <- apply(HLC(quotes), 1, mean) # HLC gets the high low and close of quote objects, average the daily price
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes)) # create an empty matrix
  for(x in 1:n.days) r[,x] <- Next(Delt(Cl(quotes),v,k=x),x)# fill matrix r, Next allows a shift in time series
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin])) # target margin +/-p%, default = 2.5% 
  if(is.xts(quotes))
    xts(x, time(quotes))
  else
    x
}

# understand behavior of T.ind visually with these steps
candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice") # newTA adds indicators to candle stick plots
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1) # on = 1 plot on first graph window.
addT.ind()

#3.3.2 Which predictors 
# use multiple "features" in the model to predict future price movements
# we will use some in the TTR library
library(TTR)

myATR <- function(x) ATR(HLC(x))[, "atr"] #Average Time Range, indicator of volitility
mySMI <- function(x) SMI(HLC(x))[, "SMI"] #Stochastic Momentum Index, indicator of momentum
myADX <- function(x) ADX(HLC(x))[, "ADX"] #Welles Wilder's Directional Index, 
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator #identifies starting trends
myBB <- function(x) BBands(HLC(x)) [, "pctB"] #Bollinger Bands - compare volatility over a period of time
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High", "Low")]))[, 1] # Chaikin volatility
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1] # close location value, how closing price relates to session range
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[, 2] #Arms' Ease of movement Value
myMACD <- function(x) MACD(Cl(x))[, 2] # MACD oscillator
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"]) #Money Flow Index
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1] #Parabolic stop and reverse
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1] # volatility indicator

#22 variables, but will try to reduce by using random forest for selection - importance of variables
# 2 methods of feature selection 1) feature filters - using statistical properties (eg. correllation)
# 2) wrappers - modelling method for selection to iteratively add and subtract features and find importance

# Our method will kind of be the wrapper method except no iterative searching
# Break data into test and train (training.per in BuildModel(), 30 years)
data(GSPC)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k = 1:10) + myATR(GSPC) + mySMI(GSPC) +
                           myADX(GSPC) + myAroon(GSPC) + myBB(GSPC) + myChaikinVol(GSPC) +
                           myCLV(GSPC) + CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC)))+ myEMV(GSPC) +
                            myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
set.seed(1234)
rf <- buildModel(data.model, method = "randomForest", 
                 training.per = c(start(GSPC),index(GSPC["1999-12-31"])), ntree = 50, importance = T)


# can specify and store a formula 
ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM), k = 1:3))
data <- modelData(ex.model, data.window = c("2009-01-01", "2009-08-10")) #produces a zoo object 
# which you can cast into a data frame or matrix
# m <- myFavoriteModellingTool(ex.model@modelformula, as.data.frame(data))

# return to rf above 
# importance = TRUE means that the model will measure variable importance with two scores:
# 1) increase in error by removing each variable, by mean square error of each tree averaged
# 2) decrease in node impurity that is accountable with each variable, again averaged over all trees

varImpPlot(rf@fitted.model, type = 1)
# now need to select a threshold on the importance, we will use 10% MSE in this case
imp <- importance(rf@fitted.model, type = 1)
imp
rownames(imp)[which(imp >10)]

# build model with variables above importance threshold
data.model <- specifyModel(T.ind(GSPC) ~ myATR(GSPC) + 
                             myADX(GSPC) + mySMI(GSPC) + myVolat(GSPC) + mySAR(GSPC))

# the T.ind function returs an xts object that is primarily for numerics, but we want to do classification
# when T > .1 = buy, T < -.1 sell, and hold
# so we need to move out of the xts to do classification with nominal predictions
# okay now build the training data and test data with data.frames and omit NAs
Tdata.train <- as.data.frame(modelData(data.model, data.window = c("1970-01-02", "1999-12-31")))
Tdata.eval<- na.omit(as.data.frame(modelData(data.model, data.window = c("2000-01-01", "2009-09-15"))))
Tform <- as.formula('T.ind.GSPC ~ .')

# use confusion matrix for evaluation of precision - proportion of events produced by model that are
# correct, & recall - the proportion of the events occuring in the domain that is signaled as such by 
# the models. These two metrics are often combined into one - F-measure

# 3.4.1 Three forms of obtaining predictions and updating/reteaching models
# - 1) single period 2) sliding window 3) growing window
# there are two ways to address a "regime shift" for sliding and growing windows:
# 1. detect when a shift happens based on the data (i.e. perfomance degradation), difficult and must not overreact
# 2. use a set update time frame/period (w), which is simpler

#3.4.2.1 Artificial Nueral Networks (ANN)
# feed forward - layered neurons that feed a linear computation and a non-linear computation, which
# produces a result that is the input to another neuron. 
# ANNs are suseptiple to different scales, so normalize variables before inputting into ANN

# simple illustration of nueral net using 1000 for net and next 1000 for predicton
set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform, norm.data[1:1000,], size = 10, decay = 0.01, maxit = 1000, linout = T, trace = F)
# initial weights of links set to randoms [-0.5 ... 0.5], two successive runs can produce differnt results. thus set seed.
# arguments: formula, data, size of hidden node layer, decay - weight updating rate, 
# maxit - max iterations, linout = regression problem
norm.preds <- predict(nn, norm.data[1001:2000,])
preds <- unscale(norm.preds, norm.data)

#evaluate the preds on trading signals with our thresholds
sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.nn, true.sigs)

# can also use  ANNs for classification, meaning probabilities for each class for every test case
# same process as above
set.seed(1234)
library(nnet)
signals <- trading.signals(Tdata.train[,"T.ind.GSPC"], 0.1, -0.1)
norm.data <- data.frame(signals = signals, scale(Tdata.train[, -1]))
nn <- nnet(signals ~ ., norm.data[1:1000,], size = 10, decay = 0.01, maxit = 1000, trace = F)
preds <- predict(nn, norm.data[1001:2000, ], type = 'class')
sigs.PR(preds, norm.data[1001:2000, 1])


# 3.4.2.2 Support Vector Machines - mapping of orignal data into a new high dimensional space, where 
# it is possible to to apply linear methods to obtain a seperating hyperplane. Seperation is done
# with kernel functions. 

# first with the regression task
library(e1071)
sv <- svm(Tform, Tdata.train[1:1000,], cost = 10)#, gamma = 0.001, cost = 1)
s.preds <- predict(sv, Tdata.train[1001:2000, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)

# next for classification
library(kernlab)
data <- cbind(signals = signals, Tdata.train[, -1])
ksv <- ksvm(signals ~ ., data[1:1000, ], C = 10)
ks.preds <- predict(ksv, data[1001:2000, ])
sigs.PR(ks.preds, data[1001:2000, 1])

# 3.4.2.3 Multivariate Adaptive Regression Splines (mars) - form of additive regression
# only applicable for regression problems, not classifcation
# packages mda:mars() or earth:earth(). Use earth as it is more inline with previous implementations
library(earth)
e <- earth(Tform, Tdata.train[1:1000,])
e.preds <- predict(e, Tdata.train[1001:2000,])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"], 0.1, -0.1)
sigs.PR(sigs.e, true.sigs)

# 3.5 Trading simulator
# see policy 1 & 2 function written in another script file

#Execute policies
# Using SVM

# train and test periods
start <- 2000 # retry with 2000 or 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
# getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date,'/',sep = '')][1:len.ts]
#learning the model and obtaining its signal predictions
library(e1071)
s <- svm(Tform, Tdata.train[tr,], cost = 10, gamma = 0.01)
p <- predict(s, Tdata.train[ts,])
sig <- trading.signals(p, 0.1, -0.1)
# now using the trading simulator
t1 <- trading.simulator(market, sig, 'policy.1', list(exp.prof=0.05, bet = 0.2, hold.time = 30))
t1
summary(t1)
#trading evaluation can be used to obtain a series of economic indicators of the performance during
# simulsation period
tradingEvaluation(t1)
# can also obtain plot to view performance
plot(t1, market, theme = "white", name = "SP500")

# try with policy 2
t2 <- trading.simulator(market, sig, 'policy.2', list(exp.prof=0.05, bet = 0.2))
t2
summary(t2)
tradingEvaluation(t2)
plot(t2, market, theme = "white", name = "SP500")
# retry with policy 2 with a different period
# DO NOT RELY ON JUST ONE TEST PERIOD, need more repitions under different conditions to ensure some
# statistical reliability of our results.

# 3.6 Model evaluation and selection

#3.6.1 Monte Carlo Estimates
# shouldn't use cross-validation methods on time series since order matters in TS
# should also use the most recent data to predict future data
# monte carlo here will iterate n times finding a point in time (from 10 to 25 years,) in the sample and then capture
# the -10years for training and the +5 years for testing. 

# experimentalComparison() can be used for Monte Carlo
# 3.6.2 Experimental Comparison
#  see test train eval funcs and Monte Carlo .R files

# Results Analysis
# Monte Carlo is way too computatively intensive. load result data files obtained from the web
# each file has results from every testtraineval cycle
load("Data/svmR.Rdata")
load("Data/svmC.Rdata")
load("Data/nnetR.Rdata")
load("Data/nnetC.Rdata")
load("Data/earth.Rdata")

# use the rank systems based on specified statistics:
# 1. precision - prec.ab (precision is important because it generates a signal by which trades are opened)
#   recall is not as important as they represent lost opportunities
# 2. Return - Ret
# 3. Return over Buy and hold - RetOverBH
# 4. Percent of profitable trades - PercProf
# 5. Risk - Sharpe ratio and Max draw down 
# subset on specific stats above
# rankSystems
tgtStats <- c('prec.sb', 'Ret', 'PercProf', 'MaxDD', 'SharpeRatio')
allSysRes <- DMwR::join(subset(svmR, stats = tgtStats), 
                  subset(svmC, stats = tgtStats),
                  subset(earth, stats= tgtStats),
                  subset(nnetR, stats = tgtStats),
                  subset(nnetC, stats = tgtStats), by = 'variants')

rankSystems(allSysRes, 5, maxs = c(T,T,T,F,T))

# some of these variants are useless as they have a precision score of 1, which is due to infrequent
# trades. These models are useless, shown here
summary(subset(svmC, stats = c('Ret','RetOverBH', 'PercProf', 'NTrades'), 
               vars = c('slide.svmC.v5', 'slide.svmC.v6')))

# need to figure out which are the best based on some criteria: Ntrades, Return, profitable trades
fullResults <- join(svmR, svmC, nnetR, nnetC, earth, by = 'variants')
nt <- statScores(fullResults, "NTrades")[[1]] # StatScores recieves a CompExp obj and provides the average score for the system
rt <- statScores(fullResults, "Ret")[[1]]
pp <- statScores(fullResults, "PercProf")[[1]]
s1 <- names(nt)[which(nt > 20)] # average more than 20 trades
s2 <- names(rt)[which(rt > 0.5)] # average more than 0.5% return
s3 <- names(pp)[which(pp > 40)] # profitable trades > 40%
namesBest <- intersect(intersect(s1, s2), s3) # find common models in all criteria
summary(subset(fullResults, stats = tgtStats, vars = namesBest)) # summary of best models that meet constaints (3/240 variants)

# CompAnalysis determines statistical significance for these results
compAnalysis(subset(fullResults, stats = tgtStats, vars = namesBest))
# can also plot to get an idea
plot(subset(fullResults, stats = c("Ret", "PercProf", "MaxDD"), vars = namesBest))
getVariant("single.nnetR.v12", nnetR) # this one seems interesting for its 2800% return on one iteration
# would probably leave this one out of the final system due to instablility, but will include for
# demonstation purposes

# 3.7 Trading System
# apply the models to the holdout data - 
data <- tail(Tdata.train, 2540)
results <- list()
for(name in namesBest){
  sys <- getVariant(name, fullResults)
  results[[name]] <- runLearner(sys, Tform, data, Tdata.eval) # not sure why I'm getting an error here
}
results <- t(as.data.frame(results))

results[, c("Ret", "RetOverBH", "MaxDD", "SharpeRatio", "NTrades", "PercProf")]
getVariant("grow.nnetR.v12", fullResults)

model <- learner("MC.nnetR", list(maxit = 750, linout = T, trace = F, size = 10, decay = 0.001))
preds <- growingWindowTest(model, Tform, data, Tdata.eval, relearn.step = 120)
signals <- factor(preds, levels = 1:3, labels = c("s", "h", "b"))
date <- row.names(Tdata.eval)[1]
market <- GSPC[paste(date, "/", sep = "")][1:length(signals),]
trade.res <- trading.simulator(market, signals, policy.func = "pol2")

plot(trade.res, market, theme = "white", name = "SP500 - Final Test")

# cool performance and risk analytics tools you can use in this library:
library(PerformanceAnalytics)
rets <- Return.calculate(trade.res@trading$Equity)                 
chart.CumReturns(rets, main = "Cumulative Returns of the strategy", ylab = "Returns")
y.returns <- yearlyReturn(xts(trade.res@trading$Equity))

plot(100*y.returns, main = "yearly percentage returns of trading system")
abline(h = 0, lty = 2)
table.CalendarReturns(xts(rets))
table.DownsideRisk(xts(rets))
