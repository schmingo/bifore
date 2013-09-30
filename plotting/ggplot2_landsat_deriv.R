################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM LANDSAT8 SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-09-30
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "latticeExtra", "reshape2", "RColorBrewer", "colorspace")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")


################################################################################
### Import data ################################################################

data.deriv <- read.csv2("src/csv/hai/hai_greyvalues_landsat8_deriv.csv",
                        dec = ".",
                        header = TRUE, stringsAsFactors = FALSE)

data <- read.csv2("src/csv/hai/hai_greyvalues_landsat8.csv",
                  dec = ".", 
                  header = TRUE, stringsAsFactors = FALSE)


################################################################################
### Subsetting #################################################################

sub.data <- melt(data[, c(1, 7:ncol(data))], id.vars = "Plotname", variable.name = "Band")
sub.data <- sub.data[order(sub.data$Plotname), ]

sub.data.deriv <- melt(data.deriv[, c(1, 7:ncol(data.deriv))], id.vars = "Plotname", variable.name = "Band")
sub.data.deriv <- sub.data.deriv[order(sub.data.deriv$Plotname), ]

sub.select.data <- subset(sub.data, Plotname %in% c("HEG01"))
sub.select.data.deriv <- subset(sub.data.deriv, Plotname %in% c("HEG01"))


################################################################################
### Lineplot ###################################################################

