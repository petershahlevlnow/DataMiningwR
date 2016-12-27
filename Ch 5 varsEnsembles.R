# Ch 5 Vars Ensemble function
# this function is used with varcluster filtering. 50 models with 30 predictors randomly selected from 
# variable clusters
# args:
# tgt target variable
# train - training data
# test - testing data
# varsSets - list containing the sets of variable names (the obtained clusters) from which we should sample
#            a variable to generate the predictors of each member of the ensemble
# baseLearner - name of learning function
# blpars - learning parameters

varsEnsembles <- function(tgt,train,test,
                          varsSets,
                          baseLearner,blPars,
                          verb=F)
{
  preds <- matrix(NA,ncol=length(varsSets),nrow=NROW(test))
  for(v in seq(along=varsSets)) {
    if (baseLearner=='knn')
      preds[,v] <- knn(train[,varsSets[[v]]],
                       test[,varsSets[[v]]],
                       train[,tgt],blPars)
    else {
      m <- do.call(baseLearner,
                   c(list(as.formula(paste(tgt,
                                           paste(varsSets[[v]],
                                                 collapse='+'),
                                           sep='~')),
                          train[,c(tgt,varsSets[[v]])]),
                     blPars)
      )
      if (baseLearner == 'randomForest')
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])],
                                  type='response'))
      else
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])]))
    }
  }
  ps <- apply(preds,1,function(x)
    levels(factor(x))[which.max(table(factor(x)))])
  ps <- factor(ps,
               levels=1:nlevels(train[,tgt]),
               labels=levels(train[,tgt]))
  if (verb) structure(ps,ensemblePreds=preds) else ps
}