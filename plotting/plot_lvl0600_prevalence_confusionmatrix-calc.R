cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot RandomForest further calculations (level0600)
##  - Accuracy
##  - Kappa
##  - POFD (Probability of false detection)
##  - POD (Probability of detection)
##  - FAR (False alarm ratio)
##  - CSI (Critical success index)
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
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/csv/kili/")


### Import dataset #############################################################

## Sorted by no.of.prevalence
data.raw <- read.csv2("lvl0600_rf_prevalence_species-cut_mean100_confusion.csv",
                         dec = ",",
                         header = TRUE,
                         stringsAsFactors = FALSE)


################################################################################
### Plotting - prevalence - all species ########################################
################################################################################

## Define output image | open image port
png("images/lvl0600_rf_prevalence_cut_confusionmatrix.png", 
    width = 2048 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)


# Line-plot
# alle im header genannten confusion matrix Berechnungen


plot <- ggplot(data.raw, aes(y=species, y=number.of.prevalence)) + 
  geom_bar(stat="identity") +
  scale_fill_grey() +
  xlab("value") +
  ylab("species") +
  ggtitle("Orthoptera prevalence - RandomForest confusion matrix") +
  theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = .5, size = 8),
        plot.title = element_text(lineheight = .8, size = 20))

plot

## Close image port
graphics.off()
