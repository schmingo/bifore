cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Construct a ROC curve with lvl0400 predictions
##  
##  Version: 2014-07-31
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
lib <- c("ROCR")

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Daten/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"
path.testing <- paste0(path.csv, "testing_100_test20_2014-07-28/")

file.in.prediction <- paste0(path.testing,"lvl_0400_prediction_20test.csv")


### Import data ################################################################

data.raw <- read.csv2(file.in.prediction,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Subset data ################################################################

## Subset prediction classes
df.predict.class <- data.raw[grep("class", data.raw$parameters), ]

# ## Reconstruct factors
# df.predict.class2 <- as.factor(ifelse(df.predict.class >= 1,"yes","no")) 
# df.predict.class2 <- factor(df.predict.class2, levels = c("yes", "no"))


## Subset prediction probabilities
df.predict.prob <- data.raw[grep("prob", data.raw$parameters), ]



