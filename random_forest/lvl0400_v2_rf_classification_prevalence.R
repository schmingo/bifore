cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Perform RandomForest classification for Orthoptera prevalence 
##  with lvl0300 dataset
##
##  1. 100 times Stratified sampling of plots 
##  2. Perform RandomForest Classification
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
Sys.setenv(LANG = "en")
## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("sampling", "foreach", "doParallel", "randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/csv/kili/")
setwd("D:/")

## Set number of CPU cores
ncores <- detectCores()-1

## Set number of RandomForest runs
rf.runs <- 100

## Runtime calculation
starttime <- Sys.time()

### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"
path.testing <- paste0(path.csv, "testing/")

file.in.0300 <- paste0(path.csv,"lvl0300_biodiversity_data.csv")
file.out.conf <- paste0(path.csv,"lvl0400_rf_prevalence_confmatrix.csv")
file.out.MDA <- paste0(path.csv,"lvl0400_rf_prevalence_varimp_MDA.csv")
file.out.MDG <- paste0(path.csv,"lvl0400_rf_prevalence_varimp_MDG.csv")

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

## Loop stratified-function 100 times
for (i in seq(1:rf.runs)) {
  # foreach (i = seq(1:10)) %do% {
  # foreach (i = 1) %do% {
  cat("\n\nRUNNING STRATIFIED DATAFRAME ", i, "\n")
  set.seed(i)
  
  ## Function call
  data.str <- stratified(data.cut, 1, 1)
  
  ## Reorder data frame
  data.str <- data.frame(cbind(data.str[, ncol(data.str)-3],
                               data.str[, 1:(ncol(data.str)-4)]))
  names(data.str) <- names(data.cut)
  
  
  ### Prepare Data for Random
  
  ## Get species list for RandomForest 
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
  
  ## Split incoming dataset
  df.greyval <- data.str[(which(names(data.str) == "greyval_band_1")):(which(names(data.str) == "greyval_band_36"))]
  
  
  ##############################################################################
  ##############################################################################
  
  
  df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %do% {
    
    ## Select species data
    df.singlespecies <- data.frame(data.str[,names(data.str) %in% c(s)])
    names(df.singlespecies) <- s
    
    tmp.species <- df.singlespecies
    
    ## Create dataframe with single species as predictor dataset
    df.spec.greyval <- cbind(df.greyval, tmp.species)
    
    
    ## Define Random Forest input data #########################################
    
    train.data <- df.spec.greyval
    
    
    ### Random Forest function #################################################
    ### Classification for single species ######################################
    
    predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
    response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])
    
    
    train.rf <- randomForest(x = predictor_modisVAL,
                             y = response_speciesCLASS,
                             importance = TRUE,
                             ntree = 500,
                             # mtry = 5,
                             nodesize = 2,
                             type="classification",
                             do.trace = FALSE)
    
    ## Variable Importance
    variableImportance <- importance(train.rf)
    
    
    ## Create a dataframe for a single species
    df.rf.singlespecies <- data.frame(c(
      train.rf$confusion[1,1],
      train.rf$confusion[1,2],
      train.rf$confusion[2,1],
      train.rf$confusion[2,2],
      train.rf$confusion[1,3],
      train.rf$confusion[2,3]))
    
    ## Set names for single species dataframe
    names(df.rf.singlespecies) <- s
    
    
    
    return(df.rf.singlespecies)
    
  }
  
  ## Set rownames for RandomForest dataframe
  df.rf.allspecies$names <- c("O0_P0",
                              "O0_P1",
                              "O1_P0",
                              "O1_P1",
                              "Class.error 0",
                              "Class.error 1")
  
  ## Reposition rownames at the beginning of RandomForest dataframe
  df.rf.allspecies <- cbind(df.rf.allspecies[, ncol(df.rf.allspecies)],
                            df.rf.allspecies[, 2:ncol(df.rf.allspecies)-1])
  names(df.rf.allspecies)[1] <- paste0("rf_", i)
  
  #   write.csv(df.filename.csv, header = FALSE, append = TRUE)
  #   Jeder durchgang wird hinten angefügt. Header kann nicht gesetzt werden,
  #   es muss zusätzlich eine spalte mit i (index des loops) hinzugefügt werden.
  
  write.table(df.rf.allspecies,
            file = paste0(path.testing, "lvl_0400_df.csv"),
#             header = FALSE,
            append = TRUE,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            dec = ",",
            sep = ";",
            )
  ### Write testing dataframe ##################################################
#   cat("\n\nWRITE TESTING DATAFRAME ", i, "\n")
#   write.table(df.rf.allspecies,
#               file = paste0(path.testing, "lvl_0400_df", i, ".csv"),
#               dec = ",",
#               quote = FALSE,
#               col.names = TRUE,
#               row.names = FALSE,
#               sep = ";")
  
}

## Runtime calulation
endtime <- Sys.time()
time <- endtime - starttime
cat("\n\nRUNTIME ", time, "seconds", "\n")