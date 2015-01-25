cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot mean kappa of level 0410 dataset
##  
##  Version: 2015-01-21
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
lib <- c()

lapply(lib, function(...) library(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Daten/")
setwd("D:/")


### Set filepaths ##############################################################

path.csv <- "Dropbox/Code/bifore/src/csv/kili/lvl0400_2015-01-24/"
path.image <- "Dropbox/Code/bifore/src/images/"
file.in <- paste0(path.csv,"lvl0410_kappa.csv")


### Import data ################################################################

data <- read.csv2(file.in, header = TRUE)


### Prepare plot ###############################################################

## Subset data
data.spec <- data[,-c(1,2)]

## Calculation of mean, standard deviation and range
mean.kappa <- sort(colMeans(data.spec), decreasing = FALSE)
sd.kappa <- apply(data.spec, 2, sd)
min.kappa <- apply(data.spec, 2, min)
max.kappa <- apply(data.spec, 2, max)

## Generating short names; for genera the first two letters to distingusih 
## genera which start with thesame letter

names.plot <- vector(mode = "character",length = length(mean.kappa))
names.split <- strsplit(as.character(names(mean.kappa)), split = "[.]")

for (i in (1: length(mean.kappa))) {
  names.plot[i] <- paste(substr(names.split[[i]][1], 1, 2),
                   ". ", names.split[[i]][2], sep = "")
}

### Plot #######################################################################

## Define output image | open image port
png(paste0(path.image, "lvl0410_meanKappa.png"), 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

user <- par(no.readonly = TRUE)
par(mar = c(4, 12, 1, 1), lwd = 1,las = 1, cex = .8)
plot(mean.kappa, 1:length(mean.kappa),
     type = "n",
     axes = T,
     yaxt="n",
     xlab = " Mean Kappa", 
     ylab = "",
     cex.lab = 1.2,
     xlim = c(min(min.kappa),
              max(max.kappa)))

#lines(min.kappa[names(mean.kappa)],1:length(mean.kappa),col="blue")
#lines(max.kappa[names(mean.kappa)],1:length(mean.kappa),col="blue")

polygon(c(max.kappa[names(mean.kappa)], rev(min.kappa[names(mean.kappa)])),
        c(1:length(mean.kappa), rev(1:length(mean.kappa))),
        col = "grey", border = NA)
polygon(c(mean.kappa+sd.kappa[names(mean.kappa)], 
        rev(mean.kappa-sd.kappa[names(mean.kappa)])),
        c(1:length(mean.kappa), rev(1:length(mean.kappa))),
        col = "darkgrey", 
        border = NA)

points(mean.kappa, 1:length(mean.kappa), pch = 21, bg = "black",cex = 1.7)

abline (v = 0.4, lwd = 1, col = "red")

axis(1,cex.axis = 1)
axis(2,at = 1:length(mean.kappa), labels = names.plot, font = 3)

par(user)

## Close image port
graphics.off()