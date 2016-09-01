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

# 2.5.4 imputation by correlation of observations through Eucledian distance
# remove too many NA rows, see line 74 "-manyNAs()"
# 1st method uses median of 10 closest neighbors and fills in median
algae1 <- knnImputation(algae1, k =10, meth = "median")

#2nd method uses weighted average of 10 closest neighbors and fills in weighted average, 
# weights decrease with distance
algae1 <- knnImputation(algae1, k = 10)

# 2.6 Obtaining prediction models
# 2.6.1 Multiple linear regressions
# using imputation from knnImputation
lm.a1 <- lm(a1 ~ ., data = algae1[, 1:12])
summary(lm.a1)
plot(lm.a1)
# "." in lm formula means all other variables in data not predictor

# backward elimination 
# anova
anova(lm.a1)
lm2.a1 <- update(lm.a1, .~. - season)
summary(lm2.a1)
# model still doesn't reduce much error, but simpler
# compare the two models 
anova(lm.a1, lm2.a1)

# try using step() default is backward elimination
final.lm <- step(lm.a1)
summary(final.lm)
# still doesn't explain much variance, so I would questions the application of linear model for this domain

# 2.6 Regression trees
# these models can handle missing data, so only eliminate manyNAs
library(rpart)
rt.a1 <- rpart(a1 ~ ., data = algae1[,1:12])
rt.a1

#apply prettyTree() part of DMwR library for formatted tree
prettyTree(rt.a1)

#trees tend to overfit, thus pruning is advised post process (2 step process)
#parameters cp, minsplit, and maxdepth can control how the tree is grown 
# prune
printcp(rt.a1)
# typically you want to choose the tree with least 1-SE (xerror) and standard dev. (xstd)
# then pick the tree with the xerror just below xerror+xstd from above
# printcp shows how the tree was grown
# to pick that tree you can prune
rt2.a1 <- prune(rt.a1, cp = 0.08)
rt2.a1

# or you can use a built in function of DMwR that automates both steps 
rt.a1 <- rpartXse(a1 ~ ., data =  algae1[,1:12])

# or you can prune with snip.rpart
first.tree <- rpart(a1 ~ ., data = algae1[,1:12])
first.tree
snip.rpart(first.tree, c(4, 7))
prettyTree(first.tree)
second.tree <- snip.rpart(first.tree, c(4, 7))
prettyTree(second.tree)

# 2.7 Model eval and selection
# using mean absolute error MAE
data(algae)
algae <- algae[-manyNAs(algae),]
clean.algae <- knnImputation(algae, k = 10)
lm3.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12])
final.lm <- step(lm3.a1)

lm.predictions.a1 <- predict(final.lm, clean.algae)
rt.predictions.a1 <- predict(rt.a1, algae1)

(mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[,"a1"])))
(mae.al.rt <- mean(abs(rt.predictions.a1 - algae[,"a1"])))

# can also use mean squared error (MSE)
(mse.a1.lm <- mean((abs(lm.predictions.a1 - algae[,"a1"]))^2))
(mse.al.rt <- mean((abs(rt.predictions.a1 - algae[,"a1"]))^2))

# disadvantage is that these aren't normalized so another method is NMSE, values less that
# 1 mean your model is predicting better than average (smaller better), larger than 1
# model predicts worse than average
(nmse.al.lm <-  mean((abs(lm.predictions.a1 - algae[,"a1"]))^2)/
               mean((mean(algae[,'a1'])-algae[,'a1'])^2))
(nmse.al.rt <-  mean((abs(rt.predictions.a1 - algae[,"a1"]))^2)/
                mean((mean(algae[,'a1'])-algae[,'a1'])^2))

# you can use the book package to do all of the above calculations
regr.eval(algae[,"a1"], rt.predictions.a1, train.y = algae[, "a1"])
regr.eval(algae[,"a1"], lm.predictions.a1, train.y = algae[, "a1"])

# visual inspection with scatter plot of errors, all points should be on the line x = y
old.par <- par(mfrow = c(1,2))
plot(lm.predictions.a1, algae[,"a1"], main = "linear model", xlab = "Predictions", ylab = "true values")
abline(0, 1, lty = 2)
plot(rt.predictions.a1, algae[,"a1"], main = "regression trees", xlab = "Predictions", ylab = "True values")
abline(0,1, lty = 2)
par(old.par)

# we can check where there are particularly poor predictions with the identify function
plot(lm.predictions.a1, algae[,"a1"], main = "linear model", xlab = "Predictions", ylab = "true values")
abline(0, 1, lty = 2)
algae[identify(lm.predictions.a1, algae[,"a1"]),] # returns rows that you select with bad predictions

# can improve model by assuming a1 can't be below zero 
sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0, 0, lm.predictions.a1)
# campare models with regr.eval
regr.eval(algae[,"a1"], lm.predictions.a1)
regr.eval(algae[,"a1"], sensible.lm.predictions.a1)

# common issue is overfitting when using all data. Need a more reliable method 
# enter k-fold CV (k fold cross validation) or bootstrapping 
# obtain k subsets of equally sized data and random data
# build models on k-1 subsets to predict k
# repeat and average results

# we can use experimentalComparison() to do the above
# we need to build functions to provide this function with necessary inputs
# user supplies the functinos of models to compare in experimentalComparison() function
# functions need to have a train+test+evaluate cycle
cv.rpart <- function(form, train, test,...)
{
  m <- rpartXse(form,train,...)
  p <- predict(m, test)
  mse <- mean((p - resp(form,test))^2)
  c(nsme=mse/mean((mean(resp(form, test)) - resp(form, test))^2))
}

cv.lm <- function(form, train, test,...)
{
  m <- lm(form,train,...)
  p <- predict(m, test)
  p <- ifelse(p < 0, 0, p)
  mse <- mean((p - resp(form,test))^2)
  c(nsme=mse/mean((mean(resp(form, test)) - resp(form, test))^2))
}

# here we've assumed that the nmse is the evaluation metric
# resp gets the target variables data

# first arg: vector = dataset(formula, data.frame, label)
# 2nd arg: vector = variants of learning systems, cv.lm used with defaults, cv.rpart used 
# with alternative se parameters defined for 3 different tree variants (i.e. four models total compared)
# 3rd arg: vector of settings defining repitions (3), k subsets (10), seed for random # 
# generator (1234)
res <- experimentalComparison(
                              c(dataset(a1 ~ ., clean.algae[, 1:12], 'a1')),
                              c(variants('cv.lm'),
                                variants('cv.rpart', se = c(0, 0.5, 1))),
                              cvSettings(3, 10, 1234))

summary(res)
plot(res)
getVariant('cv.rpart.v1', res)

# now we can carry this out for all seven prediction tasks all at once 
DSs <- sapply(names(clean.algae)[12:18], 
              function(x, names.attrs){
                f <- as.formula(paste(x, "~ ."))
                dataset(f, clean.algae[,c(names.attrs, x)], x)},
              names(clean.algae)[1:11])

resAll <- experimentalComparison(
                              DSs,
                              c(variants('cv.lm'),
                                variants('cv.rpart', se = c(0, 0.5, 1))),
                              cvSettings(5, 10, 1234))
plot(resAll)
# many bad results show up (ie nmse above 1)
# get the best scores
bestScores(resAll)

# no model predicts very well. this may be a good candidate for an ensemble approach
# ensemble approaches take a bunch of models and then taking a combination of their predictions.
# random forests are good at ensemble approaches. 
library(randomForest)
cv.rf <- function(form, train, test,...)
{
  m <- randomForest(form,train,...)
  p <- predict(m, test)
  mse <- mean((p - resp(form,test))^2)
  c(nsme=mse/mean((mean(resp(form, test)) - resp(form, test))^2))
}

resAll <- experimentalComparison(
  DSs,
  c(variants('cv.lm'),
    variants('cv.rpart', se = c(0, 0.5, 1)),
    variants('cv.rf', ntree = c(200, 500, 700))),
  cvSettings(5, 10, 1234))

bestScores(resAll)
# this doesn't tell us the confidence that another dataset will produce the same 
# we can check this with the compAnalysis function in DMwR
# is cv.rf.v3 truly the best for a1 and a2
compAnalysis(resAll, against = 'cv.rf.v3', datasets = c('a1', 'a2'))

# sig codes here are what we are looking for. It tells us if there are models significantly
# higher (plus signs) or lower (minus signs) than the model in question

# 