cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Perform Random Forest classification for Orthoptera prevalence 
##  with lvl0300 dataset
##
##  1. 100 times Stratified sampling of plots 
##  2. Perform Random Forest Classification
##  3. Extract confusion matrix & variable importance for all 100 samples and
##     average values
##  4. Perform further calculations
##     - Accuracy
##     - Kappa
##     - POFD (Probability of false detection)
##     - POD (Probability of detection)
##     - FAR (False alarm ratio)
##     - CSI (Critical success index)
##  
##  Version: 2014-06-23
##  
################################################################################
##
##  Copyright (C) 2014 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("sampling", "foreach", "doParallel", "caret", "miscTools")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/csv/kili/")
setwd("D:/")

## Set number of CPU cores
ncores <- detectCores()-1

## Set number of Random Forest runs
rf.runs <- 2

## Set size of training data (percentage) eg.: .75 for 75 %
train.part <- .8

## Set number of Random Forest trees to grow
trees <- 500

## Runtime calculation
starttime <- Sys.time()

### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"
path.testing <- paste0(path.csv, "testing/")

file.in.0300 <- paste0(path.csv,"lvl0300_biodiversity_data.csv")
file.out.rf.all <- paste0(path.testing, "lvl_0400_rf_all.csv")
file.out.rf.averaged <- paste0(path.testing, "lvl_0400_rf_averaged.csv")

if (!file.exists(path.testing)) {dir.create(file.path(path.testing))}


### Import data ################################################################

data.raw <- read.csv2(file.in.0300,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subset data ################################################################

## Remove bands containing NA values
data.raw <- cbind(data.raw[1:8],  # basics
                  data.raw[10:13],  # basics
                  data.raw[9],  # no.of.species
                  data.raw[14:178],  # species
                  data.raw[179:188],  # greyvalues
                  data.raw[197:216])  # greyvalues

# names(data.raw[179:ncol(data.raw)])

## Check greyvalues, diff and sd colums for NA values
anyNA(data.raw[179:ncol(data.raw)])


### Remove species with less than x observations in different plots ############
### Most abundant species ######################################################

## Set minimum observations in different plots
obs <- 15

data.list <- split(data.raw, data.raw$plot)
data.tmp.list <- do.call("rbind", lapply(seq(data.list), function(i) {
  matrix <- as.matrix(data.list[[i]][, 14:178])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

data.species.index <- which(apply(data.tmp.list, 
                                  2, 
                                  sum, 
                                  na.rm = TRUE) >= obs) + 13

data.cut <- data.raw[, c(1:13, data.species.index, 179:ncol(data.raw))]

# names(data.cut)

### Subsetting data ############################################################

data.cut.basics <- data.cut[, which(colnames(data.cut) == "plot"):which(colnames(data.cut) == "coordN")]
data.cut.specno <- data.cut[length(names(data.cut.basics))+1]
data.cut.species <- data.cut[, (which(colnames(data.cut) == "coordN")+2):(which(colnames(data.cut) == "greyval_band_1")-1)]
data.cut.greyval <- data.cut[, which(colnames(data.cut) == "greyval_band_1"):which(colnames(data.cut) == "greyval_band_36")]


### calculate new no.of.prevalence (cut sophisticates old no.of.species) #######

data.cut.specno <- data.frame(apply(data.cut.species,
                                    1,
                                    function(x) sum(!is.na(x[1:ncol(data.cut.species)]))))
names(data.cut.specno) <- "no.of.prevalence"

data.cut <- cbind(data.cut.basics, 
                  data.cut.specno,
                  data.cut.species,
                  data.cut.greyval)


### Calculate prevalence #######################################################

## Read species dataframe as matrix
matrix.prevalence <- as.matrix(data.cut.species)

## Replace NA with 0
matrix.prevalence[is.na(matrix.prevalence)] <- 0

## Replace values >=1 with 1
matrix.prevalence <- ifelse(matrix.prevalence >= 1,1,0)

## Recombine dataframes
data.cut <- cbind(data.cut.basics,
                  #                   data.cut.specno,
                  as.data.frame(matrix.prevalence),
                  data.cut.greyval)


### Stratified sampling ########################################################

## Function
#  df = data frame
#  class = column number of stratification variables
#  size = class size

stratified = function(df, class, size) {
  require(sampling)
  df.tmp = df[order(df[class]),]
  if (size < 1) {
    size = ceiling(table(df.tmp[class]) * size)
  } else if (size >= 1) {
    size = rep(size, times = length(table(df.tmp[class])))
  }  
  strat = strata(df.tmp, stratanames = names(df.tmp[class]), 
                 size = size, method = "srswor") #  sampling without replacement
  (dsample = getdata(df.tmp, strat))
}

## Initiate dataframe for all Random Forest runs
df.rf.outcome <- data.frame()

## Loop stratified-function 100 times
for (i in seq(1:rf.runs)) {
  # foreach (i = seq(1:10)) %do% {
  # foreach (i = 1) %do% {
  cat("\n\nPERFORM RANDOM FOREST FOR STRATIFIED DATAFRAME ", i, "OF ", rf.runs,"\n")
  set.seed(i)
  
  ## Function call
  data.str <- stratified(data.cut, 1, 1)
  
  ## Reorder data frame
  data.str <- data.frame(cbind(data.str[, ncol(data.str)-3],
                               data.str[, 1:(ncol(data.str)-4)]))
  names(data.str) <- names(data.cut)
  
  
  ### Prepare Data for Random Forest ###########################################
  
  ## Get species list for Random Forest 
  lst.species <- names(data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  lst.species
  
  ## Remove species without any observations (just to be sure ;) )
  index <- which(colSums(data.str[, lst.species]) > 0) + 
    grep("coordN", names(data.str))
  
  data.str <- data.frame(data.str[, 1:grep("coordN", names(data.str))], 
                         data.str[, index], 
                         data.str[, grep("greyval_band_1", 
                                         names(data.str))[1]:ncol(data.str)])
  
  ## Update species list
  lst.species <- names(data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  
  lst.species <- lst.species[1]
  
  ## Subset predictor variables
  df.rf.predictor <- data.str[(which(names(data.str) == "greyval_band_1")):(which(names(data.str) == "greyval_band_36"))]
  
  
  ### Loop over all species (perform Random Forest)#############################
  
  ## Parallelization
#   registerDoParallel(cl <- makeCluster(ncores))
  
  df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %do% {
    
    ## Subset response variable for each species
    df.rf.response <- data.frame(data.str[,names(data.str) %in% c(s)])
    names(df.rf.response) <- s
    
    ## Create training data for Random Forest
    train.data.all <- cbind(df.rf.predictor, df.rf.response)
    
    
    
    ## Split dataset in training and test data     
    set.seed(50)
    
    index <- sample(1:nrow(train.data.all), nrow(train.data.all)*train.part)
    
    train.data <- train.data.all[index, ]
    test.data <- train.data.all[-index, ]
    test.data.predict <- test.data[, 1:ncol(test.data)-1]
    test.data.response <- test.data[, ncol(test.data)]
    
    
    ### Random Forest function #################################################
    ### Classification for single species ######################################
    
    predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
    response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])
    
    
    train.rf <- train(x = predictor_modisVAL,
                      y = response_speciesCLASS,
                      method = "rf",
                      trControl = trainControl(method = "cv"),
                      tuneLength = 6)
    
    predict.rf <- predict(train.rf, 
                          newdata = test.data.predict)
    
    
    df.predict <- cbind(test.data.predict, predict.rf, test.data.response)
    
  }
  
#   stopCluster(cl)
  
}

## Runtime calulation
endtime <- Sys.time()
time <- endtime - starttime
cat("\n\nRUNTIME ", time, "\n")
