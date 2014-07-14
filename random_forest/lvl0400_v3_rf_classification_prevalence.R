cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Perform Random Forest classification for Orthoptera prevalence 
##  with lvl0300 dataset
##
##  1. 100 times Stratified sampling of plots
##  2. Split dataset into training and testing data
##  3. Perform Random Forest Classification
##  4. Extract confusion matrix & variable importance for all 100 samples and
##     average values
##  5. Calculations:
##     - Accuracy
##     - Kappa
##     - POFD (Probability of false detection)
##     - POD (Probability of detection)
##     - FAR (False alarm ratio)
##     - CSI (Critical success index)
##  
##  Version: 2014-07-10
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
lib <- c("sampling", 
         "foreach", 
         "doParallel", 
         "caret", 
         "e1071", 
         "randomForest",
         "miscTools")

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Daten/")
setwd("D:/")

## Set number of CPU cores
ncores <- detectCores()

## Set number of Random Forest runs
rf.runs <- 2

## Set size of training data (percentage) eg.: .75 for 75 %
train.part <- 1

## Set Random Forest tuning parameter "mtry" and "ntree"
tune.grid <- c(1,2,3,4,5,6)
# tune.grid <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
trees <- 500


## Runtime calculation
starttime <- Sys.time()

### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"
path.testing <- paste0(path.csv, "testing/")

file.in.0300 <- paste0(path.csv,"lvl0300_biodiversity_data.csv")
file.out.rf.all <- paste0(path.testing, "lvl_0400_rf_all_traindata.csv")
file.out.rf.averaged <- paste0(path.testing, "lvl_0400_rf_averaged_traindata.csv")

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
df.rf.output <- data.frame()

## Loop stratified-function 100 times
for (i in seq(1:rf.runs)) {
  # foreach (i = seq(1:10)) %do% {
  # foreach (i = 1) %do% {
  cat("\n\nPERFORM RANDOM FOREST FOR STRATIFIED DATAFRAME ", i, "OF ", rf.runs,"\n")
  #   i = 1
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
  
  
  ## Create dataframe for Random Forest
  ## (Plot, observation date, MODIS image date, greyvalues & prevalence)
  df.randomForest <- cbind(data.str[, 1:3],
                           data.str[(which(names(data.str) == "greyval_band_1")):(which(names(data.str) == "greyval_band_36"))],
                           data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  
  ## Prepare tuning parameters for Random Forest function call
  tune.grid <- data.frame(tune.grid)
  names(tune.grid) <- "mtry"
  
  
  ### Split dataset in training and test data ##################################
  
  set.seed(50)  # Todo: Check if this seed always needs to be the same - if not, plot selection is always different -> set.seed(i)
  
  index <- sample(1:nrow(df.randomForest), nrow(df.randomForest)*train.part)
  
  df.rf.train <- df.randomForest[index, ]
  df.rf.test <- df.randomForest[-index, ]
  
  ## Subset training dataset
  df.rf.train.basics <- data.frame(df.rf.train[, 1:3])
  df.rf.train.predict <- df.rf.train[(which(names(df.rf.train) == "greyval_band_1")):(which(names(df.rf.train) == "greyval_band_36"))]
  df.rf.train.response <- df.rf.train[(which(names(df.rf.train) == "greyval_band_36")+1):ncol(df.rf.train)]
  
  ## Subset test dataset
  df.rf.test.basics <- data.frame(df.rf.test[, 1:3])
  df.rf.test.predict <- df.rf.test[(which(names(df.rf.test) == "greyval_band_1")):(which(names(df.rf.test) == "greyval_band_36"))]
  df.rf.test.response <- df.rf.test[(which(names(df.rf.test) == "greyval_band_36")+1):ncol(df.rf.test)]
  
  
  ### Loop over all species (perform Random Forest) ############################
  
  ## Parallelization
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
  lst.species <- lst.species[1:5]
    df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %dopar% {
#   df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %do% {
    tmp.df.singlespecies <- data.frame()
    
    
    ## Get response variable as factor
    tmp.rf.train.response <- as.factor(df.rf.train.response[, names(df.rf.train.response) %in% c(s)])
    tmp.rf.test.response <- as.factor(df.rf.test.response[, names(df.rf.test.response) %in% c(s)])
    
    
    ### Random Forest function #################################################
    ### Classification for single species ######################################
    
    tmp.train.rf <- train(x = df.rf.train.predict,
                          y = tmp.rf.train.response,
                          method = "rf",
                          #                       trControl = trainControl(method = "cv"),  # Causes warning message
                          tuneGrid = tune.grid,
                          ntree = trees)
    
    
    #     tmp.train.rf$finalModel$confusion
    #     best.mtry <- tmp.train.rf$bestTune[[1]]
    #     tmp.train.rf$bestTune[2]
    #     tmp.train.rf$perfNames
    #     tmp.train.rf$results[3,1]
    #     tmp.train.rf$results[tmp.train.rf$bestTune[[1]],2] #  Accuracy
    #     tmp.train.rf$results[tmp.train.rf$bestTune[[1]],3] #  Kappa
    #     tmp.train.rf$results[tmp.train.rf$bestTune[[1]],4] #  AccuracySD
    #     tmp.train.rf$results[tmp.train.rf$bestTune[[1]],5] #  KappaSD
    #     tmp.train.rf$trainingData
    #     tmp.train.rf$resample
    
    if (train.part != 1) {
      
      ## Predict
      tmp.test.predict.rf <- predict.train(tmp.train.rf, 
                                           newdata = df.rf.test.predict)
      
      ## Calculate Confusion Matrix from test data
      tmp.confMatrix <- confusionMatrix(data = tmp.test.predict.rf,
                                        reference = tmp.rf.test.response,
                                        dnn = c("Predicted", "Observed"))
      
      
    } else {
      ## Extract Prediction values from train data
      tmp.train.predict.rf <- data.frame(tmp.train.rf$finalModel[3])
      tmp.train.predict.rf <- as.factor(tmp.train.predict.rf[, 1])
      
      ## Calculate Confusion Matrix from train data
      tmp.confMatrix <- confusionMatrix(data = tmp.train.predict.rf,
                                        reference = tmp.rf.train.response,
                                        dnn = c("Predicted", "Observed"))
      
    }
    
    ## Extract Confusion Matrix values
    tmp.O0_P0 <- tmp.confMatrix$table[1]
    tmp.O0_P1 <- tmp.confMatrix$table[2]
    tmp.O1_P0 <- tmp.confMatrix$table[3]
    tmp.O1_P1 <- tmp.confMatrix$table[4]
    tmp.sum_obs <- sum(tmp.O0_P0, 
                       tmp.O0_P1, 
                       tmp.O1_P0, 
                       tmp.O1_P1)
    tmp.class.error0 <- tmp.O1_P0/sum(tmp.O0_P0, 
                                      tmp.O1_P0)
    tmp.class.error1 <- tmp.O0_P1/sum(tmp.O1_P1, 
                                      tmp.O0_P1)
    tmp.Accuracy <- tmp.confMatrix$overall[1]
    tmp.Kappa <- tmp.confMatrix$overall[2]
    tmp.AccuracyLower <- tmp.confMatrix$overall[3]
    tmp.AccuracyUpper <- tmp.confMatrix$overall[4]
    tmp.AccuracyNull <- tmp.confMatrix$overall[5]
    tmp.AccuracyPValue <- tmp.confMatrix$overall[6]
    tmp.McnemarPValue <- tmp.confMatrix$overall[7]
    tmp.Rsquared <- (sum(tmp.O0_P0, tmp.O1_P1))/tmp.sum_obs
    
    ## Write extracted values into a dataframe
    tmp.df.singlespecies <- data.frame(rbind(tmp.O0_P0,
                                             tmp.O0_P1,
                                             tmp.O1_P0,
                                             tmp.O1_P1,
                                             tmp.sum_obs,
                                             tmp.class.error0,
                                             tmp.class.error1,
                                             tmp.Accuracy,
                                             tmp.Kappa,
#                                              tmp.AccuracyLower,
#                                              tmp.AccuracyUpper,
#                                              tmp.AccuracyNull,
#                                              tmp.AccuracyPValue,
#                                              tmp.McnemarPValue,
                                             tmp.Rsquared))
    
    ## Set colnames (species name)
    names(tmp.df.singlespecies) <- s
    
    return(tmp.df.singlespecies)
  }
  
      stopCluster(cl)
  
  df.rf.allspecies$rf_run <- i
  
  ## Write rownames to single column
  df.rf.allspecies$parameters <- rownames(df.rf.allspecies)
  
  
  ## Write dataframe for all species
  df.rf.allspecies2 <- cbind(df.rf.allspecies$rf_run,
                             df.rf.allspecies$parameters,
                             df.rf.allspecies[1:(ncol(df.rf.allspecies)-2)]) 
  
  
  ## Append Random Forest output in a single dataframe
  df.rf.output <- rbind(df.rf.output, df.rf.allspecies2)
  
}

### Write Random Forest output dataframe #######################################
cat("\n\nWRITE RANDOM FOREST OUTPUT DATAFRAME (ALL RF RUNS)\n")
write.table(df.rf.output,
            file = file.out.rf.all,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

### Create averaged Random Forest output dataframe #############################

## Initialize averaged dataframe
df.rf.averaged <- data.frame(names(df.rf.output[3:ncol(df.rf.output)]))
names(df.rf.averaged) <- "species"

## Average Confusion Matrix
tmp.names <- df.rf.output[1:nrow(df.rf.allspecies), 2]
for(i in tmp.names) {
  tmp.df.sub <- subset(df.rf.output, tmp.names == i)
  tmp.means <- data.frame(colMeans(tmp.df.sub[, 3:ncol(tmp.df.sub)]))
  names(tmp.means) <- i
  df.rf.averaged <- cbind(df.rf.averaged, tmp.means)
}

## Upate column names (remove "tmp.")
for(i in seq(2, (length(names(df.rf.averaged))), 1)) {
  new.name <- strsplit(names(df.rf.averaged)[i], "tmp.")
  names(df.rf.averaged)[i] <- new.name[[1]][2]
}


## Write averaged Random Forest dataframe
cat("\n\nWRITE RANDOM FOREST AVERAGED DATAFRAME\n")
write.table(df.rf.averaged,
            file = file.out.rf.averaged,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

## Runtime calulation
endtime <- Sys.time()
time <- endtime - starttime
cat("\n\nRUNTIME ", time, "\n")


