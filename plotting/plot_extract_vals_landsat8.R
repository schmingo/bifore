################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-25
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
setwd("hier_kommt_der_Flo ;-)") # Linux
setwd("hier_kommt_der_Flo ;-)") # Windows


## Import files
table.hai.land8 <- "src/csv/hai_greyvalues_landsat8.csv$"

