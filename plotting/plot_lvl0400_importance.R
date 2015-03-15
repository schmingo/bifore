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
##     - Confusion Matrix
##     - Classification Errors
##     - Accuracy
##     - Kappa
##     - Sensitivity
##     - Specificity
##     - DetectionRate
##  
##  Version: 2015-02-14
##  
################################################################################
##
##  Copyright (C) 2015 Simon Schlauss (sschlauss@gmail.com)
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
lib <- c()

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/sschlauss/")
setwd("D:/")

### Set filepaths ##############################################################

path.csv                  <- "Code/bifore/src/csv/"
path.testing              <- paste0(path.csv, "lvl0400_2015-01-24/")

file.in.importance        <- paste0(path.testing,"lvl0400_importance_25test.csv")


### Import data ################################################################

data.raw <- read.csv2(file.in.importance,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### prepare data ###############################################################

## remove varimp rank from dataset
df.varImp <- data.raw[-which(data.raw$parameters %in% data.raw$parameters[31:60]), ]

## reorder dataset
# df.varImp1 <- df.varImp[order(df.varImp$parameters),]

lst.bands   = unique(df.varImp$parameters)
lst.species = names(df.varImp[3:ncol(df.varImp)])
rf.runs     = unique(df.varImp$RandomForest_run)


df.all <- data.frame()
tmp.df.spec <- data.frame()
tmp.vec <- c()

for(s in seq(1:length(lst.species))) {
  
    ##
    for(b in seq(1:length(lst.bands))) {
      
        lst.id = which(df.varImp$parameters %in% lst.bands[b])
        
        ## 
        for(i in rf.runs) {
          tmp <- df.varImp[lst.id[i],s+2]
          tmp 
          tmp.vec = c(tmp.vec,tmp)
        }
        tmp.vec.mean <- mean(tmp.vec)
        tmp.df.spec <- rbind(tmp.df.spec,tmp.vec.mean)
    }
    df.all <- cbind(df.all,tmp.df.spec)
}