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

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("sampling", "foreach", "doParallel")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/csv/kili/")
setwd("D:/")



### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/"

file.in.0300 <- paste0(path.csv,"lvl0300_biodiversity_data.csv")
file.out.conf <- paste0(path.csv,"lvl0400_rf_prevalence_confmatrix.csv")
file.out.MDA <- paste0(path.csv,"lvl0400_rf_prevalence_varimp_MDA.csv")
file.out.MDG <- paste0(path.csv,"lvl0400_rf_prevalence_varimp_MDG.csv")


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
                  data.raw[197:216]  # greyvalues
#                   data.raw[217:226],  # diff
#                   data.raw[236:254],  # diff
#                   data.raw[255:264],  # sd
#                   data.raw[273:292]   # sd
                  )

names(data.raw[179:ncol(data.raw)])

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

data <- data.raw[, c(1:13, data.species.index, 179:ncol(data.raw))]

# names(data)


### Stratified sampling ########################################################
# stratified(df, id, group, size, seed="NULL", ...)
unique(data$plot)

set.seed(20)

foreach (i = seq(1:100)) %do% {
  set.seed(20)
  s <- sample(1:1000, 100)
  set.seed(s)
  data2 <- data[strata(data, 
                      stratanames = "plot", 
                      size = rep(1,length(unique(data$plot))),
                      method = "srswor")$ID_unit, ]
  return(data$nr.of.species[2])
}
# nrow(data)
# ncol(data)



# Tim Appelhans: for (i in 1:iters) {
#   cat("\n\nRUNNING", mthd, ": ITERATION", i, "\n")
#   set.seed(i)
#   
#   ind.eval <- sample(nrow(plots.ta.monthly.rug), 
#                      nrow(plots.ta.monthly.rug) * train.size)
#   ta.pred <- plots.ta.monthly.rug[ind.eval, ]
#   
#   
#   train.size = 0.8
#   plots.ta.monthly.rug = meinem data