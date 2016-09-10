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
  for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x) # fill matrix r, Next allows a shift in time series
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
Tdata.eval<- na.omit(as.data.frame(modelData(data.model, data.window = c("1970-01-02", "1999-12-31"))))
Tform <- as.formula('T.ind.GSPC ~ .')

# use confusion matrix for evaluation of precision - proportion of events produced by model that are
# correct, & recall - the proportion of the events occuring in the domain that is signaled as such by 
# the models

