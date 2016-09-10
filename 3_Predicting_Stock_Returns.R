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

# 