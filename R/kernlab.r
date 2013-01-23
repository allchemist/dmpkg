#################################################################
### kernlab package
### interested functionality: kfa, kha, kkmeans, kpca, ksvm,
###                           lssvm, rsvm, specc 
#################################################################

library (kernlab)

#################################################################
##### ksvm: Support Vector Mashine

### main options

## call by formula 
# x      = formula
# data   = dataframe for formula

## call by data
# x      = matrix
# y      = vector of outputs

## common options
# scaled = internal scaling (bool)
# type   = classification/regressions/etc
# kernel = kernel type
# kpar   = list of kernel parameters

## a lot of tune options. try ?ksvm

### usage
# net   -> ksvm(formula, train.data, ...)
# pred  -> predict(net, test.inputs)
# table(pred, test.outputs)
# summary(net)

## comments
# train.data is full dataframe

# test.outputs is a dataframe outputs column (number n)
# syntax: test.data[,n] or outputs(test.data)

# test.inputs is a dataframe without outputs column
# syntax: test.data[,-n] or inputs(test.data)


#################################################################
##### lssvm: Least Squares Support Vetor Machine

### usage same as ksvm, but several differences. try ?lssvm


#################################################################
##### rvm: Relevance Vetor Machine

### usage same as ksvm, but several differences. try ?rvm


#################################################################
##### kfa: Kernel Feature Analysis

### main options

## call by formula
# x         = formula
# data      = dataframe for formula

## call by data
# x         = matrix

## common options
# kernel    = kernel type
# kpar      = list of kernel parameters
# features  = number of features (principal components) to return
# subset    = number of features used from data
# normalize = normalize selected features (bool)

### usage
# net -> kfa(formula, data, features, ...)
# pred -> predict(net, newdata)


#################################################################
##### kpca: Kernel Principal Component Analysis

### usage same as kfa, but several differences

## no 'subset' option

## new options
# th = smallest step size


#################################################################
##### kha: Kernel Hebbian Algorithm

### usage same as kfa, but several differences

## new options
# eta     = hebbian learning rate
# maxiter = max number of iterations
# verbose = report every 100 iters (bool)


#################################################################
##### kkmeans: Kernel K-means

### main options

## call by formula
# x = formula
# data = dataframe

## call by data
# x = matrix

## common options
# centers = number of clusters of initial set of clusters
# kernel = kernel type
# kpar = list of kernel parameters
# algorithm = kkmeans or kernighan

## some tune options. try ?kkmeans

### usage
# sc -> kkmeans(as.matrix(data), centers=n)
# centers(sc)

#################################################################
##### specc: Spectral Clustering

### usage same as kkmeans, but several differences

## no 'algorithm' option

## a lot of tune options. try ?specc



#################################################################
##### kernlab + patterns wrapper

setGeneric ('kernlab.call', function(func, pats, ...) standardGeneric('kernlab.call'))

setMethod ('kernlab.call', c('function', 'dataframe.patterns'),
           function(func, pats, ...) do.call(func, list(x=pats@formula, data=pats@data, ...)))

setMethod ('kernlab.call', c('function', 'matrix.patterns'),
           function(func, pats, ...) do.call(func, list(x=data@formula, y=pats@outputs, ...)))

### example usage:

# kernlab.call(kfa, pats, features=..., kernel=...)
