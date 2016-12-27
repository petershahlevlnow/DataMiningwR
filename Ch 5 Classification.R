# Chapter 5 Classifcation of Microarray Samples
# try to predict disease states using gene levels (variables) from observations/samples (individuals)

# 5.2 Get Data and packages from Biaconductor website (need to only do this once)
source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("ALL")
biocLite("genefilter")
biocLite("hgu95av2.db")

# use the data set, 
library(Biobase)
library(ALL)
data("ALL") # gets a special class from Biobase
ALL

# get specific cases of BT and mio.biol factors
tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & 
                     ALL$mol.biol %in% levels(ALL$mol.biol)[1:4])
ALLb <- ALL[,tgt.cases] #data set is trasposed (observations are columns, and variables are rows)
ALLb

ALLb$BT <- factor(ALLb$BT)
ALLb$mol.biol <- factor(ALLb$mol.biol)
save(ALLb, file = "Data/myALL.Rdata")

# 5.2.1 explore data
es <- exprs(ALLb)
dim(es)

# this data set has 12000+ variables which makes modelling difficult, need to reduce dimensions
summary(as.vector(es))
# veiw visually, shorth is the mean of the interquartile range
library(genefilter)
hist(as.vector(es), breaks = 80, prob = T, xlab = 'Expression Levels',
     main = 'Histogram of Overall Expression Levels')
abline(v = c(median(as.vector(es)), 
             shorth(as.vector(es)), 
             quantile(as.vector(es), c(0.25, 0.75))),
       lty=2, col=c(2,3,4,4))
legend('topright', c('Median', 'Shorth', '1stQ', '3rd Q'),
       lty = 2, col = c(2, 3, 4, 4))

# are the distributions fo the gene expression levels of the samples with a particular mutation 
# different from each other?
# the following code answers this question:
sapply(levels(ALLb$mol.biol), function(x) summary(as.vector(es[, which(ALLb$mol.biol == x)])))

# 5.3 Gene (Feature) Selection
# generally two types of filters:
# 1) filters - single step, statistical approach 
# 2) wrappers - uses data mining tools, which run filters+model+evaluate cycle several times until
# some converege. computationally intensive for large data sets

# 5.3.1 Simple Filters based on Distribution Properties
# finding genes(variables) with little variablity or not expressed at all and can be safely removed
# get an overall idea of distribution of the expression levels of genes with the following graph
# using the IQR and median, RowMedian and RowQ(quartile) from Biobase which is more efficient than using apply function
# remember rows are the variables here.
rowIQRs <- function(em) rowQ(em, ceiling(0.75*ncol(em)))-rowQ(em,floor(0.25*ncol(em)))
plot(rowMedians(es), rowIQRs(es), 
     xlab = 'Median Expression Level',
     ylab = 'IQR Expression Level',
     main = 'Main Characteristics of Gene Expression Levels')

# from the plot we observe that much of the genes have low IQRs, which will not be useful for 
# discriminating among the different types mutations.
# there is a caveat here that we are looking at genes individually so if combined with other 
# genes could be useful

# we will filter out genes with a variability smaller than 1/5 of the global variability 
library(hgu95av2.db)
ALLb <- nsFilter(ALLb,
                 var.func=IQR,
                 var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
ALLb
ALLb <- ALLb$eset
es <- exprs(ALLb)
dim(es)

# 5.3.2 ANOVA Filters
# genes that have means across multiple samples can be discarded
f <- Anova(ALLb$mol.biol, p =0.01) #creates a new function for carrying out Anova filtering, requires a factor
ff <- filterfun(f) # filtering function for a expression matrix
selGenes <- genefilter(exprs(ALLb), ff)    # creates a vector of True False values for genes that don't have similar means

length(selGenes)
sum(selGenes)
ALLb <- ALLb[selGenes, ]
ALLb

es <- exprs(ALLb)
plot(rowMedians(es), rowIQRs(es), 
     xlab = 'Median Expression Level',
     ylab = 'IQR Expression Level',
     main = 'Main Characteristics of Gene Expression Levels')

# 5.3.3 Filtering using a Random Forest
# can use this to rank features' importance

featureNames(ALLb) <- make.names(featureNames(ALLb)) # make names to make standard data frame name
es <- exprs(ALLb)

library(randomForest)
dt <- data.frame(t(es), Mut = ALLb$mol.biol) # training set
rf <- randomForest(Mut ~ ., dt, importance = T)
imp <- importance(rf)
imp <- imp[, ncol(imp) - 1]
rf.genes <- names(imp)[order(imp, decreasing = T)[1:30]] # top 30 genes

sapply(rf.genes, function(g) tapply(dt[ ,g], dt$Mut, median))

# plot using a level plot 
library(lattice)
ordMut <- order(dt$Mut)
levelplot(as.matrix(dt[ordMut,rf.genes]),
          aspect='fill', xlab='', ylab='',
          scales=list(
            x=list(
              labels=c('+','-','*','|')[as.integer(dt$Mut[ordMut])],
              cex=0.7,
              tck=0)
          ),
          main=paste(paste(c('"+"','"-"','"*"','"|"'),
                           levels(dt$Mut)
          ),
          collapse='; '),
          col.regions=colorRampPalette(c('white','orange','blue'))
          )

# there are differences between expression levels. 

# 5.3.4 Filtering using Clustering Ensembles
# grouping variables into similar groups. 
# Ensemble methods rely on some form of diversity among individual models. One way to do this 
# is by randomly sampling predictor variavbles
library(Hmisc)
vc <- varclus(t(es))
clus30 <- cutree(vc$hclust,30)
table(clus30)

# here we randomly sample variables from 30 clusters. Each time we call this it will produce a
# different gene set. We can then model using a ensemble method with this technique.
getVarsSet <- function(cluster,nvars=30,seed=NULL,verb=F) 
{
  if (!is.null(seed)) set.seed(seed)
  
  cls <- cutree(cluster,nvars)
  tots <- table(cls)
  vars <- c()
  vars <- sapply(1:nvars,function(clID)
  {
    if (!length(tots[clID])) stop('Empty cluster! (',clID,')')
    x <- sample(1:tots[clID],1)
    names(cls[cls==clID])[x]
  })
  if (verb)  structure(vars,clusMemb=cls,clusTots=tots)
  else       vars
}
getVarsSet(vc$hclust)
getVarsSet(vc$hclust)

# 5.4 Predicting Cytogenetic Abnormalities
# task of predicting type of cytogenetic abnormaliteis of the B-cell ALL cases.

# 5.4.1 Prediction Task
# 1) model consisting of genes(predictors) and known B-cell Values (target). 
# 2) gene selection will change with each iteration of model similar to section 5.3
# 3) multiclass classification problem. target can have one of four values

# 5.4.2 Evaluation Metric
# classification problems like this can have several eval metrics:
# 1) error rate or complement - accuracy
# 2) Area under ROC curve
# 3) precision and recall
# 4) measures of accuracy of the class probability estimates (Brier Score) 

# ROC curves have drawbacks with multiple classifications
# here we will use standard accuracy:
# acc = 1 - 1/N * sum(1:N) of Loss function 
# where loss function is L[0,1] {0 yi = pyi, 1 otherwise} (basically 0 = correct classification, 1 is not correct)

# 5.4.3 Experimental Procedure - Leave one out cross validation (LOOCV)
# Obtain N models (94 in this case), where each model uses N-1 samples and tested on left out sample
# illustration using the iris data set
data(iris)
rpart.loocv <- function(form, train, test, ...){
  require(rpart, quietly = T)
  m <- rpart(form, train,...)
  p <- predict(m, test, type = 'class')
  c(accuracy = ifelse(p == resp(form, test), 100, 0))
}

# loocv from book package
library(DMwR)
exp <- loocv(learner('rpart.loocv', list()), dataset(Species ~ .,iris), 
             loocvSettings(seed = 1234, verbose = F))

summary(exp)

# 5.4.4 Modelling Techniques
# We will use three data sets - one with genes of the ANOVA filter, and other two with 30 gene selection
# Three modelling techniques:
# 1) Random forest - where prediction is tallied votes among all trees, as opposed to regression where averaging across all trees to obtain prediction
# 2) k nearest neighbor - measures distance and similarity between samples, similar gene mutations
# 3) SVM - non linear relationship among gene expressions and abnormalities

# 5.4.4.2 k-nearest neighbor
# doesn't make a model. looks for similarities among stored data set to make the prediction
# in classification this is a voting mechanism, therefore odd numbers is desirable
# can also include distance
# normalizing data is important if including distance
# simple illustration

library(class)
data(iris)
idx <- sample(1:nrow(iris), as.integer(0.7 * nrow(iris))) # sample 70% of iris for train
tr <- iris[idx, ]
ts <- iris[-idx, ] # test data is everthing not in sample
preds <- knn(tr[ , -5], ts[ , -5], tr[ , 5], k = 3)
table(preds, ts[, 5])

# small function that standardizies numerics and then does an KNN - this function is included in
# the DMwR package so need to execute it.
kNN <- function(form, train, test, norm = T, norm.stats = NULL, ...){
  require(class, quietly = TRUE)
  tgtCol <- which(colnames(train) == as.character(form[2]))
  if (norm){
    if(is.null(norm.stats))
      tmp <- scale(train[, -tgtCol], center = T, scale = T)
    else # if norm stats of centrality and spread statistics are provided as a list
      tmp <- scale(train[, -tgtCol], center = norm.stats[[1]], scale = norm.stats[[2]])
    train[, -tgtCol] <- tmp
    ms <- attr(tmp, "scaled:center")
    ss <- attr(tmp, "scaled:scale")
    test[, -tgtCol] <- scale(test[, -tgtCol], center = ms, scale = ss)
  }
  knn(train[, -tgtCol], test[, -tgtCol], train[, tgtCol], ...)
}

# test KNN function with normalization and without normalization
preds.norm <- kNN(Species ~ ., tr, ts, k = 3)
table(preds.norm, ts[, 5])

preds.notNorm <- kNN(Species ~ ., tr, ts, norm = F, k = 3)
table(preds.notNorm, ts[, 5])

# 5.4.5 Comparing the Madels
# feature selection will be done as part of the modelling process to avoid bias
# for each iteration of the LOOCV, a feature selection process is carried out
# the only feature selection done prior to model input is the simple filtering

# the below lists define the variants of each model including the feature selection method
vars <- list()
vars$randomForest <- list(ntree = c(500, 750, 100),
                          mtry = c (5, 15,30),
                          fs.meth = list(list('all'),
                                         list('rf', 30),
                                         list('varclus', 30, 50)))
vars$svm <- list(cost = c(1, 100, 500),
                 gamma = c (0.01, 0.001, 0.0001),
                 fs.meth = list(list('all'),
                                list('rf', 30),
                                list('varclus', 30, 50)))
vars$knn <-  list(k = c(3, 5, 7, 13),
                  norm = c (T, F),
                  fs.meth = list(list('all'),
                                 list('rf', 30),
                                 list('varclus', 30, 50)))
