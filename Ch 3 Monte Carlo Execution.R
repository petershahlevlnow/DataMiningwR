# Chapter 3 Monte Carlo execution code. this will take a long time to run as is. 

# The list of learners we will use
TODO <- c('svmR', 'svmC', 'earth', 'nnetR', 'nnetC')
# The datasets used in the comparison
DSs <- list(dataset(Tform, Tdata.train, 'SP500'))
# Monte Carlo settings
# 20 repititions, ~10 years of train dailys, ~5 years of test dailys, and random generator seed. 
MCsetts <- mcSettings(20, 2540, 1270, 1234) 

# Variants to try for all learners
VARS <- list()
VARS$svmR <- list(cost = c(10, 150), gamma = c(0.01, 0.001), policy.func = c('pol1', 'pol2', 'pol3'))
VARS$svmC <- list(cost = c(10, 150), gamma = c(0.01, 0.001), policy.func = c('pol1', 'pol2', 'pol3'))
VARS$earth <- list(nk = c(10, 17), degree = c(1, 2), 
                   thresh = c(0.01, 0.001), policy.func = c('pol1', 'pol2', 'pol3'))
VARS$nnetR <- list(linout = T, maxit = 750, size = c(5,10), decay = c(0.001, 0.01), 
                  policy.func = c('pol1', 'pol2', 'pol3'))
VARS$nnetC <- list(maxit = 750, size = c(5,10), decay = c(0.001, 0.01), 
                   policy.func = c('pol1', 'pol2', 'pol3'))