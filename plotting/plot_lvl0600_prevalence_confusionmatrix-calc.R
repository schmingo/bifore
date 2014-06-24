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
lib <- c("ggplot2", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Code/bifore/src/")
setwd("D:/Dropbox/Code/bifore/src/")

## Set filenames/-paths
file.in <- "csv/kili/lvl0600_rf_prevalence_species-cut_mean100_confusion.csv"
image.out <- "images/lvl0600_rf_prevalence_cut_confusionmatrix.png"


### Import dataset #############################################################

## Sorted by no.of.prevalence
df.confusion <- read.csv2(file.in,
                          dec = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)

## Keep order by no.of.prevalence in ggplot (just to make sure)
df.confusion$species <- factor(df.confusion$species, 
                               levels=unique(df.confusion$species), 
                               ordered=TRUE)


### Subset / merge data ########################################################

## Select 10 species with highest no.of.prevalence
df.confusion <- df.confusion[1:10, ]

## Select varables to plot
attach(df.confusion)

df.confusion.sub <- data.frame(species, 
                               Accuracy,
                               Kappa,
                               POFD
#                                CSI,
#                                FAR,
#                                POD
                               )

detach(df.confusion)


## Melt dataframe
df.confusion.sub.melt <- melt(df.confusion.sub, id = "species")

# names(df.confusion.sub.melt)
# max(df.confusion.sub.melt$value)
# min(df.confusion.sub.melt$value)


### Plotting ###################################################################

## Define output image | open image port
png(image.out, 
    width = 2048 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

ggplot(data = df.confusion.sub.melt,
       aes(y = species, x = value, colour = variable, group = variable)) +
  geom_line() +
  xlab("value") +
  ylab("species") +
  xlim(0, 1) +
  ggtitle("Orthoptera prevalence - RandomForest") +
  theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = 0, size = 11),
        plot.title = element_text(lineheight = .8, size = 20),
        legend.title=element_blank())

## Close image port
graphics.off()





