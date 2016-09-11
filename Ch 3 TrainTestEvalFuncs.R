# 3.6.2 train+test+evaluate cycle functions
# set of functions that execute the full train test evaluate cycle of different trading systems.
# called from monte carlo routines

#monte carlo svm regression 
MC.svmR <- function(form, train, test, b.t = 0.1, st.t = -0.1,...){
  require(e1071)
  t <- svm(form, train, ...)
  p <- predict(t, test)
  trading.signals(p, b.t, s.t)
} #formula, training data, test data, buy/sell thresholds

#svm classification
MC.svmC <- function(form, train, test, b.t = 0.1, st.t = -0.1,...){
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[, tgtName] <- trading.signals(train[,tgtName], b.t, s.t)
  t <- svm(form, train, ...)
  p <- predict(t, test)
  factor(p, levels = c("s", "h", "b"))
}

# nnet regression
MC.nnetR <- function(form, train, test, b.t = 0.1, st.t = -0.1,...){
  require(nnet)
  t <- nnet(form, train, ...)
  p <- predict(t, test)
  trading.signals(p, b.t, s.t)
}

# nnet classification
MC.nnetC <- function(form, train, test, b.t = 0.1, st.t = -0.1,...){
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[, tgtName] <- trading.signals(train[,tgtName], b.t, s.t)
  t <- nnet(form, train, ...)
  p <- predict(t, test, type = "class")
  factor(p, levels = c("s", "h", "b"))
}

# MARS 
MC.earth <- function(form, train, test, b.t = 0.1, st.t = -0.1,...){
  require(earth)
  t <- earth(form, train, ...)
  p <- predict(t, test)
  trading.signals(p, b.t, s.t)
}

# single period
single <- function(form, train, test, learner, policy.func,...){
  p <- do.call(paste("MC", learner, sep = "."), list(form, train, test,...))
  eval.stats(form, train, test, p, policy.func = policy.func)
}

# sliding window periods
sliding <- function(form, train, test, learner, relearner.step, policy.func,...){
  real.learner <- learner(paste("MC", learner, sep = "."), pars = list(...))
  p <- slidingWindowTest(real.learner, form, train, test, relearn.step)
  p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
  eval.stats(form, train, test, p, policy.func = policy.func)
}

# growing window test
grow <- function(form, train, test, learner, relearner.step, policy.func,...){
  real.learner <- learner(paste("MC", learner, sep = "."), pars = list(...))
  p <- growingWindowTest(real.learner, form, train, test, relearn.step)
  p <- factor(p, levels = 1:3, labels = c("s", "h", "b"))
  eval.stats(form, train, test, p, policy.func = policy.func)
}

#evaluation statistics called from the above functions
eval.stats <- function(form, train, test, preds, b.t = 0.1, s.t = -0.1, ...){
  #Signals evaluations
  tgtName <- all.vars(form)[1]
  test[,tgtName] <- trading.signals(test[,tgtName], b.t, s.t)
  st <- sigs.PR(preds, test[, tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec', 'rec'), each = 3), c('s', 'h', 'b'), sep = '.')
  
  #Trading evaluation
  date <- rownames(test)[1]
  market <- GSPC[paste(date, "/", sep = '')][1:length(preds),]
  trade.res <- trading.simulator(market, preds, ...)
  c(st, tradingEvaluation(trade.res))
}