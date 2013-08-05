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
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
#setwd("Flo")

