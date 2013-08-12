################################################################################
## BiFoRe Scripts
##
## RANDOM FOREST
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-25
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest", "maptools", "raster", "rgdal", "sp")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
#setwd("Flo")

## Import dataset
data <- read.csv2("src/csv/all_greyvalues_modis_NA_abundance.csv", dec = ".",
                  header = TRUE, stringsAsFactors = FALSE)

