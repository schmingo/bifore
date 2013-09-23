################################################################################
## BiFoRe Scripts
##
## RANDOM FOREST
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-09-22
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest", "foreach", "doSNOW", "parallel")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Google Drive/bifore/") # Linux

## Import dataset
data <- read.csv2("src/csv/hai/hai_greyvalues_landsat8_abundance.csv", 
                  dec = ".", header = TRUE, stringsAsFactors = FALSE)

## Select data for randomForest
attach(data) 
train.data <- data.frame(Plotname,
#                          Plotid,
#                          Status,
#                          Location,
#                          Longitude,
#                          Latitude,
                         B01,
                         B02,
                         B03,
                         B04,
                         B05,
                         B06,
                         B07,
                         B08,
                         B09,
                         B10,
                         B11,
                         BQA,
                         abundance
                         )
detach(data)
names(train.data)

################################################################################
### Random Forest ##############################################################
?? randomForest

n.cores <- detectCores() # detect cpu cores for parallelization

## Define desired parameters
n.tree <- 500 # Number of trees to grow
m.try <- 7 # Number of variables randomly sampled as candidates at each split

## Function (parallelized)

################################################################################
# do.trace:   If set to TRUE, give a more verbose output as randomForest is run. 
#             If set to some integer, then running output is printed for every 
#             do.trace trees.
# 
# na.action:  A function to specify the action to be taken if NAs are found. 
#             (NOTE: If given, this argument must be named.)
################################################################################

parRandomForest <- function(xx, ..., ntree=n.tree, mtry=m.try, importance=TRUE, do.trace=100, 
                            na.action=na.omit, ncores=n.cores, seed=47) {
  ## Initialize Cluster
  cl <- makeCluster(ncores)
  ## Initialize RNG and distribute streams to nodes
  if(!is.null(seed)) 
    clusterSetRNGStream(cl, seed)
  ## Load randomForest package on cluster
  clusterEvalQ(cl, library(randomForest))
  
  ## randomForest function for parLapply
  rfwrap <- function(xx, ntree, ...)
    randomForest(x=xx, ntree=ntree, ...)
  ## Execute randomForest
  rfpar <- parLapply(cl, rep(ceiling(ntree/ncores), ncores), xx=xx, rfwrap, ...)
  
  ## Stop cluster
  stopCluster(cl)
  
  ## Combine resulting randomForest objects
  do.call(combine, rfpar)
}

## Call function

################################################################################
# BUG -> NA not permitted in predictors <- should work for landsat8 (no NA's)
# Assigned to Issue #4

# Error in checkForRemoteErrors(val) : 
#   4 nodes produced errors; first error: Can not handle categorical predictors with more than 32 categories.
################################################################################


system.time(train.rf <- parRandomForest(train.data[,3:ncol(train.data)-1], 
                                        train.data[ , names(train.data) %in% c("abundance")],
                                        ntree=n.tree, 
                                        mtry=m.try, 
                                        importance=TRUE, 
                                        na.action=na.omit,
                                        do.trace=50)
            )

randomForest(train.data[,3:ncol(train.data)-1], 
                train.data[ , names(train.data) %in% c("abundance2")],
                ntree=n.tree, 
                mtry=m.try, 
                importance=TRUE, 
                na.action=na.omit,
                do.trace=100)