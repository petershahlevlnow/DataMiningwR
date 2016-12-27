# Ch 5 genericModel function
# generic model function carries out the task of filtering, modeling, and predicting.

genericModel <- function(form,train,test,
                         learner,
                         fs.meth,
                         ...)
{
  cat('=')
  tgt <- as.character(form[[2]])
  tgtCol <- which(colnames(train)==tgt)
  
  # Anova filtering  
  f <- Anova(train[,tgt],p=0.01)
  ff <- filterfun(f)
  genes <- genefilter(t(train[,-tgtCol]),ff)
  genes <- names(genes)[genes]
  train <- train[,c(tgt,genes)]
  test <- test[,c(tgt,genes)]
  tgtCol <- 1
  
  # Specific filtering 
  if (fs.meth[[1]]=='varclus') {
    require(Hmisc,quietly=T)
    v <- varclus(as.matrix(train[,-tgtCol]))
    VSs <- lapply(1:fs.meth[[3]],function(x)
      getVarsSet(v$hclust,nvars=fs.meth[[2]]))
    pred <- varsEnsembles(tgt,train,test,VSs,learner,list(...))
    
  } else {
    if (fs.meth[[1]]=='rf') {
      require(randomForest,quietly=T)
      rf <- randomForest(form,train,importance=T)
      imp <- importance(rf)
      imp <- imp[,ncol(imp)-1]
      rf.genes <- names(imp)[order(imp,decreasing=T)[1:fs.meth[[2]]]]
      train <- train[,c(tgt,rf.genes)]
      test <- test[,c(tgt,rf.genes)]
    }
    
    if (learner == 'knn') 
      pred <- kNN(form,
                  train,
                  test,
                  norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                                  rowIQRs(t(as.matrix(train[,-tgtCol])))),
                  ...)
    else {
      model <- do.call(learner,c(list(form,train),list(...)))
      pred <- if (learner != 'randomForest') predict(model,test)
      else predict(model,test,type='response')
    }
    
  }
  
  c(accuracy=ifelse(pred == resp(form,test),100,0))
}