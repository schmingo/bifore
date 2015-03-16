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
lib <- c("dplyr")

lapply(lib, function(...) require(..., character.only = TRUE))


### Set filepaths ##############################################################

## Set working directory
setwd("D:/")

path.csv                  <- "Code/bifore/src/csv/lvl0400_2015-01-24/"
file.in.importance        <- paste0(path.csv,"lvl0400_importance_25test.csv")
file.out.singlespec.mean  <- paste0(path.csv,"lvl0400_importance_mean_singlespec.csv")
file.out.allspec.mean     <- paste0(path.csv,"lvl0400_importance_mean_allspec.csv")


### Import data ################################################################

data.raw <- read.csv2(file.in.importance,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### prepare data ###############################################################

## remove varimp rank from dataset
df.varImp <- data.raw[-which(data.raw$parameters %in% data.raw$parameters[31:60]), ]

## Add leading zero to bandnames
for(i in seq(1:nrow(df.varImp))) {
  tmp.str <- unlist(strsplit(df.varImp$parameters[i],"_"))
  if (nchar(tmp.str[3]) < 2) {
    tmp.str[3] <- paste0("0",tmp.str[3])
  }
  df.varImp$parameters[i] <- paste(tmp.str[1], 
                                   tmp.str[2], 
                                   tmp.str[3], 
                                   sep = "_")
}


## Get mean variable importance (100 RF runs, 30 MODIS bands, 34 species)
df.varImp %>% 
  group_by(parameters) %>% 
  summarise_each(funs(mean), -c(1, 2)) %>%
  data.frame() -> df.varImp.mean




## Calculate mean variable importance (all species)
df.varImp.mean.all <- data.frame(cbind(df.varImp.mean[,1],
                                       rowMeans(df.varImp.mean[2:ncol(df.varImp.mean)])))
names(df.varImp.mean.all) <- c("Parameter", "Mean_VariableImportance")

### plot data ##################################################################




### write csv ##################################################################

# write.table(df.varImp.mean,
#             file = file.out.singlespec.mean,
#             quote = FALSE,
#             col.names = TRUE,
#             row.names = FALSE,
#             sep = ";",
#             dec = ",")
# 
# write.table(df.varImp.mean.all,
#             file = file.out.allspec.mean,
#             quote = FALSE,
#             col.names = TRUE,
#             row.names = FALSE,
#             sep = ";",
#             dec = ",")
