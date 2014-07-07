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
lib <- c("sampling", "foreach", "doParallel", "randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/csv/kili/")
setwd("D:/")

## Set number of CPU cores
ncores <- detectCores()-1

## Set number of Random Forest runs
rf.runs <- 5

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
  cat("\n\nPERFORM RANDOM FOREST FOR STRATIFIED DATAFRAME ", i, "OF ", rf.runs,"\n")
  set.seed(i)
  
  ## Function call
  data.str <- stratified(data.cut, 1, 1)
  
  ## Reorder data frame
  data.str <- data.frame(cbind(data.str[, ncol(data.str)-3],
                               data.str[, 1:(ncol(data.str)-4)]))
  names(data.str) <- names(data.cut)
  
  
  ### Prepare Data for Random Forest ############################################
  
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
  
  ## Subset predictor variables
  df.rf.predictor <- data.str[(which(names(data.str) == "greyval_band_1")):(which(names(data.str) == "greyval_band_36"))]
  
  
  ### Loop over all species (perform Random Forest)##############################
  
  df.rf.allspecies <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %do% {
    
    ## Subset response variable for each species
    df.rf.response <- data.frame(data.str[,names(data.str) %in% c(s)])
    names(df.rf.response) <- s
    
    ## Create training data for Random Forest
    train.data <- cbind(df.rf.predictor, df.rf.response)
    
        
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
      train.rf$confusion[1,1], #  Confusion Matrix Observed 0, Predicted 0
      train.rf$confusion[1,2], #  Confusion Matrix Observed 0, Predicted 1
      train.rf$confusion[2,1], #  Confusion Matrix Observed 1, Predicted 0
      train.rf$confusion[2,2], #  Confusion Matrix Observed 1, Predicted 1
      train.rf$confusion[1,3], #  Classification Error 0
      train.rf$confusion[2,3], #  Classification Error 1
      variableImportance[1,3], #  MeanDecreaseAccuracy greyvalue MODIS band 01
      variableImportance[2,3], #  MeanDecreaseAccuracy greyvalue MODIS band 02
      variableImportance[3,3], #  MeanDecreaseAccuracy greyvalue MODIS band 03
      variableImportance[4,3], #  MeanDecreaseAccuracy greyvalue MODIS band 04
      variableImportance[5,3], #  MeanDecreaseAccuracy greyvalue MODIS band 05
      variableImportance[6,3], #  MeanDecreaseAccuracy greyvalue MODIS band 06
      variableImportance[7,3], #  MeanDecreaseAccuracy greyvalue MODIS band 07
      variableImportance[8,3], #  MeanDecreaseAccuracy greyvalue MODIS band 08
      variableImportance[9,3], #  MeanDecreaseAccuracy greyvalue MODIS band 09
      variableImportance[10,3], #  MeanDecreaseAccuracy greyvalue MODIS band 10
      variableImportance[11,3], #  MeanDecreaseAccuracy greyvalue MODIS band 17
      variableImportance[12,3], #  MeanDecreaseAccuracy greyvalue MODIS band 18
      variableImportance[13,3], #  MeanDecreaseAccuracy greyvalue MODIS band 19
      variableImportance[14,3], #  MeanDecreaseAccuracy greyvalue MODIS band 20
      variableImportance[15,3], #  MeanDecreaseAccuracy greyvalue MODIS band 21
      variableImportance[16,3], #  MeanDecreaseAccuracy greyvalue MODIS band 22
      variableImportance[17,3], #  MeanDecreaseAccuracy greyvalue MODIS band 23
      variableImportance[18,3], #  MeanDecreaseAccuracy greyvalue MODIS band 24
      variableImportance[19,3], #  MeanDecreaseAccuracy greyvalue MODIS band 25
      variableImportance[20,3], #  MeanDecreaseAccuracy greyvalue MODIS band 26
      variableImportance[21,3], #  MeanDecreaseAccuracy greyvalue MODIS band 27
      variableImportance[22,3], #  MeanDecreaseAccuracy greyvalue MODIS band 28
      variableImportance[23,3], #  MeanDecreaseAccuracy greyvalue MODIS band 29
      variableImportance[24,3], #  MeanDecreaseAccuracy greyvalue MODIS band 30
      variableImportance[25,3], #  MeanDecreaseAccuracy greyvalue MODIS band 31
      variableImportance[26,3], #  MeanDecreaseAccuracy greyvalue MODIS band 32
      variableImportance[27,3], #  MeanDecreaseAccuracy greyvalue MODIS band 33
      variableImportance[28,3], #  MeanDecreaseAccuracy greyvalue MODIS band 34
      variableImportance[29,3], #  MeanDecreaseAccuracy greyvalue MODIS band 35
      variableImportance[30,3], #  MeanDecreaseAccuracy greyvalue MODIS band 36
      variableImportance[1,4], #  MeanDecreaseGini greyvalue MODIS band 01
      variableImportance[2,4], #  MeanDecreaseGini greyvalue MODIS band 02
      variableImportance[3,4], #  MeanDecreaseGini greyvalue MODIS band 03
      variableImportance[4,4], #  MeanDecreaseGini greyvalue MODIS band 04
      variableImportance[5,4], #  MeanDecreaseGini greyvalue MODIS band 05
      variableImportance[6,4], #  MeanDecreaseGini greyvalue MODIS band 06
      variableImportance[7,4], #  MeanDecreaseGini greyvalue MODIS band 07
      variableImportance[8,4], #  MeanDecreaseGini greyvalue MODIS band 08
      variableImportance[9,4], #  MeanDecreaseGini greyvalue MODIS band 09
      variableImportance[10,4], #  MeanDecreaseGini greyvalue MODIS band 10
      variableImportance[11,4], #  MeanDecreaseGini greyvalue MODIS band 17
      variableImportance[12,4], #  MeanDecreaseGini greyvalue MODIS band 18
      variableImportance[13,4], #  MeanDecreaseGini greyvalue MODIS band 19
      variableImportance[14,4], #  MeanDecreaseGini greyvalue MODIS band 20
      variableImportance[15,4], #  MeanDecreaseGini greyvalue MODIS band 21
      variableImportance[16,4], #  MeanDecreaseGini greyvalue MODIS band 22
      variableImportance[17,4], #  MeanDecreaseGini greyvalue MODIS band 23
      variableImportance[18,4], #  MeanDecreaseGini greyvalue MODIS band 24
      variableImportance[19,4], #  MeanDecreaseGini greyvalue MODIS band 25
      variableImportance[20,4], #  MeanDecreaseGini greyvalue MODIS band 26
      variableImportance[21,4], #  MeanDecreaseGini greyvalue MODIS band 27
      variableImportance[22,4], #  MeanDecreaseGini greyvalue MODIS band 28
      variableImportance[23,4], #  MeanDecreaseGini greyvalue MODIS band 29
      variableImportance[24,4], #  MeanDecreaseGini greyvalue MODIS band 30
      variableImportance[25,4], #  MeanDecreaseGini greyvalue MODIS band 31
      variableImportance[26,4], #  MeanDecreaseGini greyvalue MODIS band 32
      variableImportance[27,4], #  MeanDecreaseGini greyvalue MODIS band 33
      variableImportance[28,4], #  MeanDecreaseGini greyvalue MODIS band 34
      variableImportance[29,4], #  MeanDecreaseGini greyvalue MODIS band 35
      variableImportance[30,4])) #  MeanDecreaseGini greyvalue MODIS band 36
    
    ## Set names for single species dataframe
    names(df.rf.singlespecies) <- s
    
    
    
    return(df.rf.singlespecies)
    
  }
  
  ## Set rownames for Random Forest dataframe
  df.rf.allspecies$names <- c("O0_P0",
                              "O0_P1",
                              "O1_P0",
                              "O1_P1",
                              "Class.error 0",
                              "Class.error 1",
                              "MDA_band01",
                              "MDA_band02",
                              "MDA_band03",
                              "MDA_band04",
                              "MDA_band05",
                              "MDA_band06",
                              "MDA_band07",
                              "MDA_band08",
                              "MDA_band09",
                              "MDA_band10",
                              "MDA_band17",
                              "MDA_band18",
                              "MDA_band19",
                              "MDA_band20",
                              "MDA_band21",
                              "MDA_band22",
                              "MDA_band23",
                              "MDA_band24",
                              "MDA_band25",
                              "MDA_band26",
                              "MDA_band27",
                              "MDA_band28",
                              "MDA_band29",
                              "MDA_band30",
                              "MDA_band31",
                              "MDA_band32",
                              "MDA_band33",
                              "MDA_band34",
                              "MDA_band35",
                              "MDA_band36",
                              "MDG_band01",
                              "MDG_band02",
                              "MDG_band03",
                              "MDG_band04",
                              "MDG_band05",
                              "MDG_band06",
                              "MDG_band07",
                              "MDG_band08",
                              "MDG_band09",
                              "MDG_band10",
                              "MDG_band17",
                              "MDG_band18",
                              "MDG_band19",
                              "MDG_band20",
                              "MDG_band21",
                              "MDG_band22",
                              "MDG_band23",
                              "MDG_band24",
                              "MDG_band25",
                              "MDG_band26",
                              "MDG_band27",
                              "MDG_band28",
                              "MDG_band29",
                              "MDG_band30",
                              "MDG_band31",
                              "MDG_band32",
                              "MDG_band33",
                              "MDG_band34",
                              "MDG_band35",
                              "MDG_band36")
  
  ## Reposition rownames at the beginning of Random Forest dataframe
  df.rf.allspecies <- cbind(df.rf.allspecies[, ncol(df.rf.allspecies)],
                            df.rf.allspecies[, 2:ncol(df.rf.allspecies)-1])
  names(df.rf.allspecies)[1] <- paste0("rf_", i)
  
  write.table(df.rf.allspecies,
              file = paste0(path.testing, "lvl_0400_df.csv"),
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
cat("\n\nRUNTIME ", time, "\n")