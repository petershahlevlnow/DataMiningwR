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
# grouping variables into similar groups
library(Hmisc)
vc <- varclus(t(es))
clus30 <- cutree(vc$hclust,30)
table(clus30)


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
