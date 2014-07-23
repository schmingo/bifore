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
##  5. Model validation:
##     - R squared
##     - Accuracy
##     - Kappa
##     - POFD (Probability of false detection)
##     - POD (Probability of detection)
##     - FAR (False alarm ratio)
##     - CSI (Critical success index)
##  
##  Version: 2014-07-23
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
         "miscTools",
         "modeest",
         "ROCR")

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Daten/")
setwd("D:/")

## Set number of CPU cores
ncores <- detectCores()

## Set number of Random Forest runs
rf.runs <- 5

## Set size of training data (percentage) eg.: .75 for 75 %
## Note: If "1" is used, prediction and confusion matrix will be
##       taken from train function!
train.part <- .8

## Set Random Forest tuning parameter "mtry" and "ntree"
mtrys <- c(1,2,3)
# mtrys <- c(1,2,3,4,5,6,7,8,9,10)
# mtrys <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
trees <- 500


## Runtime calculation
starttime <- Sys.time()

### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"
path.testing <- paste0(path.csv, "testing/")

file.in.0300 <- paste0(path.csv,"lvl0300_biodiversity_data.csv")
file.out.rf.all <- paste0(path.testing, "lvl_0400_rf_all_20test.csv")
file.out.rf.validation <- paste0(path.testing, "lvl0400_rf_validation_20test.csv")
file.out.data.cut <- paste0(path.testing, "lvl0400_data_cut.csv")
file.out.data.RF <- paste0(path.testing, "lvl0400_data_RandomForest.csv")
file.out.data.RF.test <- paste0(path.testing, "lvl0400_data_RandomForest_test.csv")

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
obs <- 20

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
matrix.prevalence <- ifelse(matrix.prevalence >= 1,"yes","no")

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
  
  #   ## Remove species without any observations (just to be sure ;) )
  #   index <- which(colSums(data.str[, lst.species]) > 0) + 
  #     grep("coordN", names(data.str))
  #   
  #   data.str <- data.frame(data.str[, 1:grep("coordN", names(data.str))], 
  #                          data.str[, index], 
  #                          data.str[, grep("greyval_band_1", 
  #                                          names(data.str))[1]:ncol(data.str)])
  #   
  #   ## Update species list
  #   lst.species <- names(data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  
  
  ## Create dataframe for Random Forest
  ## (Plot, observation date, MODIS image date, greyvalues & prevalence)
  df.randomForest <- cbind(data.str[, 1:3],
                           data.str[(which(names(data.str) == "greyval_band_1")):(which(names(data.str) == "greyval_band_36"))],
                           data.str[(which(names(data.str) == "coordN")+1):(which(names(data.str) == "greyval_band_1")-1)])
  
  ## Prepare tuning parameters for Random Forest function call
  mtrys <- data.frame(mtrys)
  names(mtrys) <- "mtry"
  
  
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
  #   cl <- makeCluster(ncores)
  #   registerDoParallel(cl)
  lst.species <- lst.species[1:3]
  df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %do% {
    
    ## Initialize dataframe
    tmp.df.singlespecies <- data.frame()
    
    ## Get response variable as factor
    tmp.rf.train.response <- as.factor(df.rf.train.response[, names(df.rf.train.response) %in% c(s)])
    tmp.rf.train.response <- factor(tmp.rf.train.response, levels = c("yes", "no"))
    tmp.rf.test.response <- as.factor(df.rf.test.response[, names(df.rf.test.response) %in% c(s)])
    tmp.rf.test.response <- factor(tmp.rf.test.response, levels = c("yes", "no"))
    
    
    ### Random Forest function #################################################
    ### Classification for single species ######################################
    
    tmp.train.rf <- train(x = df.rf.train.predict,
                          y = tmp.rf.train.response,
                          method = "rf",
                          # trControl = trainControl(method = "cv"),  # Causes warning message
                          tuneGrid = mtrys,
                          metric = "Kappa",
                          ntree = trees)
    
    if (train.part != 1) {
      
      ## Predict
      tmp.test.predict.rf <- predict.train(tmp.train.rf, 
                                           newdata = df.rf.test.predict)
      
      ## Calculate Confusion Matrix from test data
      tmp.confMatrix <- confusionMatrix(data = tmp.test.predict.rf,
                                        reference = tmp.rf.test.response,
                                        dnn = c("Predicted", "Observed"),
                                        positive = "yes")
      
    } else {
      ## Extract Prediction values from train data
      tmp.train.predict.rf <- data.frame(tmp.train.rf$finalModel[3])
      tmp.train.predict.rf <- as.factor(tmp.train.predict.rf[, 1])
      
      ## Calculate Confusion Matrix from train data
      tmp.confMatrix <- confusionMatrix(data = tmp.train.predict.rf,
                                        reference = tmp.rf.train.response,
                                        dnn = c("Predicted", "Observed"),
                                        positive = "yes")
      
    }
    
    ## Extract Confusion Matrix values
    tmp.Ono_Pno <- tmp.confMatrix$table[1]
    tmp.Ono_Pyes <- tmp.confMatrix$table[2]
    tmp.Oyes_Pno <- tmp.confMatrix$table[3]
    tmp.Oyes_Pyes <- tmp.confMatrix$table[4]
    tmp.sum_Pyes <- sum(tmp.Ono_Pyes, tmp.Oyes_Pyes)
    tmp.sum_Pno <- sum(tmp.Ono_Pno, tmp.Oyes_Pno)
    tmp.sum_Ono <- sum(tmp.Ono_Pno, tmp.Ono_Pyes)
    tmp.sum_Oyes <- sum(tmp.Oyes_Pno, tmp.Oyes_Pyes)
    tmp.sum_obs <- sum(tmp.Ono_Pno, 
                       tmp.Ono_Pyes, 
                       tmp.Oyes_Pno, 
                       tmp.Oyes_Pyes)
    tmp.class.error0 <- tmp.Oyes_Pno/sum(tmp.Ono_Pno, 
                                      tmp.Oyes_Pno)
    tmp.class.error1 <- tmp.Ono_Pyes/sum(tmp.Oyes_Pyes, 
                                      tmp.Ono_Pyes)
    tmp.mtry <- tmp.train.rf$bestTune[1,1]
    tmp.Accuracy <- tmp.confMatrix$overall[1]
    tmp.Kappa <- tmp.confMatrix$overall[2]
    tmp.AccuracyLower <- tmp.confMatrix$overall[3]
    tmp.AccuracyUpper <- tmp.confMatrix$overall[4]
    tmp.AccuracyNull <- tmp.confMatrix$overall[5]
    tmp.AccuracyPValue <- tmp.confMatrix$overall[6]
    tmp.McnemarPValue <- tmp.confMatrix$overall[7]
    tmp.Rsquared <- (sum(tmp.Ono_Pno, tmp.Oyes_Pyes))/tmp.sum_obs
    tmp.Sensitivity <- tmp.confMatrix$byClass[1]
    tmp.Specificity <- tmp.confMatrix$byClass[1]
    tmp.DetectionRate <- tmp.confMatrix$byClass[6]
    tmp.POD <- tmp.Oyes_Pyes/tmp.sum_Pyes
    tmp.FAR <- tmp.Ono_Pyes/tmp.sum_Pyes
    tmp.CSI <- tmp.Oyes_Pyes/sum(tmp.Oyes_Pyes, tmp.Oyes_Pno, tmp.Ono_Pyes)
    tmp.POFD <- tmp.Ono_Pyes/tmp.sum_Ono
    
    
    ## Get Variable Importance from Random Forest function call
    tmp.varimp <- varImp(tmp.train.rf, scale = TRUE)
    tmp.df.varimp <- data.frame(tmp.varimp$importance)
    
    ## Set speciesname as column name
    names(tmp.df.varimp) <- s
    
    ## Modify rownames in variable importance dataframe
    for (v in seq(1:nrow(tmp.df.varimp))) {
      tmp.rowname <- strsplit(rownames(tmp.df.varimp)[v], "greyval_")
      tmp.rowname <- paste0("varImp_", tmp.rowname[[1]][2])
      rownames(tmp.df.varimp)[v] <- tmp.rowname
    }
    
    ## Create dataframe for variable importance ranking (1 = "most important")
    tmp.df.varimp_rank <- data.frame(rank(-tmp.df.varimp[,1], na.last = TRUE))
    
    ## Create rownames for ranking dataframe
    rownames(tmp.df.varimp_rank) <- rownames(tmp.df.varimp)
    for (v in seq(1:nrow(tmp.df.varimp_rank))) {
      tmp.rowname <- strsplit(rownames(tmp.df.varimp_rank)[v], "_")
      tmp.rowname <- paste(tmp.rowname[[1]][1],
                           "rank",
                           tmp.rowname[[1]][2],
                           tmp.rowname[[1]][3],
                           sep = "_")
      rownames(tmp.df.varimp_rank)[v] <- tmp.rowname
    }
    
    ## Set speciesname as column name
    names(tmp.df.varimp_rank) <- s
    
    
    ## Write extracted values into a dataframe
    tmp.df.singlespecies <- data.frame(rbind(tmp.Ono_Pno,
                                             tmp.Ono_Pyes,
                                             tmp.Oyes_Pno,
                                             tmp.Oyes_Pyes,
                                             tmp.sum_Ono,
                                             tmp.sum_Oyes,
                                             tmp.sum_Pno,
                                             tmp.sum_Pyes,
                                             tmp.sum_obs,
                                             tmp.class.error0,
                                             tmp.class.error1,
                                             tmp.mtry,
                                             tmp.Accuracy,
                                             tmp.Kappa,
                                             # tmp.AccuracyLower,
                                             # tmp.AccuracyUpper,
                                             # tmp.AccuracyNull,
                                             # tmp.AccuracyPValue,
                                             # tmp.McnemarPValue,
                                             tmp.Sensitivity,
                                             tmp.Specificity,
                                             tmp.DetectionRate,
                                             tmp.Rsquared,
                                             tmp.POD,
                                             tmp.FAR,
                                             tmp.CSI,
                                             tmp.POFD))
    
    ## Set colnames (species name)
    names(tmp.df.singlespecies) <- s
    
    tmp.df.singlespecies <- rbind(tmp.df.singlespecies, 
                                  # tmp.df.varimp, 
                                  tmp.df.varimp_rank)
    
    
    return(tmp.df.singlespecies)
  }
  
  #   stopCluster(cl)
  
  df.rf.allspecies$rf_run <- i
  
  ## Write rownames to single column
  df.rf.allspecies$parameters <- rownames(df.rf.allspecies)
  
  
  ## Write dataframe for all species
  df.rf.allspecies2 <- cbind(df.rf.allspecies[ncol(df.rf.allspecies)-1],
                             df.rf.allspecies[ncol(df.rf.allspecies)],
                             df.rf.allspecies[1:(ncol(df.rf.allspecies)-2)]) 
  
  
  ## Append Random Forest output in a single dataframe
  df.rf.output <- rbind(df.rf.output, df.rf.allspecies2)
  
}

## Write Random Forest output dataframe
cat("\n\nWRITE RANDOM FOREST OUTCOME DATAFRAME (ALL RF RUNS)\n")
write.table(df.rf.output,
            file = file.out.rf.all,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")


### Create final model validation statistics dataframe #########################

## Initialize dataframe for model validataion (Get species names)
df.rf.validation <- data.frame(names(df.rf.output[3:ncol(df.rf.output)]))
names(df.rf.validation) <- "species"


## Get confusion matrix sums

## Select parameters for final model validation (get sums from confusion matrix)
tmp.names <- as.character(df.rf.output[1:9, 2])

for(i in tmp.names) {
  ## Subset rows containing specific parameter (i)
  tmp.parameter <- df.rf.output[which(df.rf.output$parameters == i), ]
  
  ## Sum parameter for each species
  tmp.sums <- data.frame(colSums(tmp.parameter[, 3:ncol(tmp.parameter)]))
  
  ## Rename parameter
  i <- strsplit(i, "tmp.")
  names(tmp.sums) <- i[[1]][2]
  
  ## Bind calculated sums
  df.rf.validation <- cbind(df.rf.validation, tmp.sums)
  
}

## Calculate model validation statistics

## Classification Error 0
df.rf.validation$class.error0 <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                         .combine = "rbind") %do% {
                                           err.0.tmp <- (df.rf.validation[i, "Oyes_Pno"]) /
                                             sum((df.rf.validation[i, "Ono_Pno"]),
                                                 (df.rf.validation[i, "Oyes_Pno"]))
                                           
                                           return(err.0.tmp)
                                         }

## Classification Error 1
df.rf.validation$class.error1 <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                         .combine = "rbind") %do% {
                                           err.1.tmp <- (df.rf.validation[i, "Ono_Pyes"]) /
                                             sum((df.rf.validation[i, "Ono_Pyes"]),
                                                 (df.rf.validation[i, "Oyes_Pyes"]))
                                           
                                           return(err.1.tmp)
                                         }

## Rsquared
df.rf.validation$Rsquared <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                     .combine = "rbind") %do% {
                                       rsquared.tmp <- ((df.rf.validation[i, "Oyes_Pyes"]) +
                                                          (df.rf.validation[i, "Ono_Pno"])) /
                                         (df.rf.validation[i, "sum_obs"])
                                       
                                       return(rsquared.tmp)
                                     }


## Observed Accuracy
df.rf.validation$observedAccuracy <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                             .combine = "rbind") %do% {
                                               acc.obs.tmp <- ((df.rf.validation[i, "Oyes_Pyes"]) +
                                                                 (df.rf.validation[i, "Ono_Pno"])) /
                                                 (df.rf.validation[i, "sum_obs"])
                                               
                                               return(acc.obs.tmp)
                                             }

## Expected Accuracy
df.rf.validation$expectedAccuracy <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                             .combine = "rbind") %do% {
                                               acc.ex.tmp <- ((df.rf.validation[i, "sum_Oyes"] * df.rf.validation[i, "sum_Pyes"] / df.rf.validation[i, "sum_obs"]) +
                                                                (df.rf.validation[i, "sum_Ono"] * df.rf.validation[i, "sum_Pno"] / df.rf.validation[i, "sum_obs"])) / 
                                                                df.rf.validation[i, "sum_obs"]
                                               return(acc.ex.tmp)
                                             }

## Kappa
df.rf.validation$Kappa <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                  .combine = "rbind") %do% {
                                    kappa.tmp <- (df.rf.validation[i, "observedAccuracy"] - df.rf.validation[i, "expectedAccuracy"]) /
                                      (1 - df.rf.validation[i, "expectedAccuracy"])
                                    return(kappa.tmp)
                                  }

## Probability of detection (POD)
df.rf.validation$POD <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                .combine = "rbind") %do% {
                                  POD.tmp <- (df.rf.validation[i,"Oyes_Pyes"]) / 
                                    (df.rf.validation[i,"sum_Oyes"])
                                  return(POD.tmp)
                                }

## False alarm ratio (FAR)
df.rf.validation$FAR <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                .combine = "rbind") %do% {
                                  FAR.tmp <- (df.rf.validation[i,"Ono_Pyes"]) / 
                                    (df.rf.validation[i,"sum_Pyes"])
                                  return(FAR.tmp)
                                }

## Critical success index (CSI)
df.rf.validation$CSI <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                .combine = "rbind") %do% {
                                  CSI.tmp <- (df.rf.validation[i,"Oyes_Pyes"]) / 
                                    (df.rf.validation[i,"Oyes_Pyes"] + 
                                       df.rf.validation[i,"Oyes_Pno"] + 
                                       df.rf.validation[i,"Ono_Pyes"])
                                  return(CSI.tmp)
                                }

## Probability of false detection (POFD)
df.rf.validation$POFD <- foreach(i = seq(1:nrow(df.rf.validation)), 
                                 .combine = "rbind") %do% {
                                   POFD.tmp <- (df.rf.validation[i,"Ono_Pyes"]) / 
                                     (df.rf.validation[i,"sum_Ono"])
                                   return(POFD.tmp)
                                 }


## Variable Importance (Mode)
# lst.ranks <- as.vector(rownames(df.rf.allspecies2)[(nrow(df.rf.allspecies2)-29):nrow(df.rf.allspecies2)])
# lst.ranks
# lst.species <- as.vector(rownames(df.rf.validation))
# 
# for (r in lst.ranks) {
#   r = lst.ranks[1]
#   tmp.rank <- df.rf.output[which(df.rf.output$parameters == r), ]
#   for ( s in 3:ncol(tmp.rank)) {
#     s=3
#     tmp.rank.vector <- as.vector(tmp.rank[, s])
#     tmp.rank.mode <- mlv(tmp.rank.vector, method = "mfv")
#     tmp.rank.mode <- tmp.rank.mode$M
#     
#   }
#     
# }


## Write final Random Forest model validation statistics dataframe
cat("\n\nWRITE FINAL RANDOM FOREST MODEL STATISTICS DATAFRAME\n")
write.table(df.rf.validation,
            file = file.out.rf.validation,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

## Runtime calulation
endtime <- Sys.time()
time <- endtime - starttime
cat("\n\nRUNTIME ", time, "\n")
