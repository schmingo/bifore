################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## EXTRACT CALCULATED CLOUD DATES                                             ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-20                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
# install.packages("MODIS", repos="http://R-Forge.R-project.org")
lib <- c("modiscloud", "devtools", "doParallel", "rgdal", "raster", "ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("d:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

path.nocloud.csv <- ("csv/kili/biodiversity_data_cloudchecked.csv")

################################################################################
### Import biodiversity dataset ################################################

data <- read.csv2(path.nocloud.csv,
#                   dec = ".",
                  header = TRUE, 
                  stringsAsFactors = TRUE)


data.cloud <- data[c(2,4,3)]
