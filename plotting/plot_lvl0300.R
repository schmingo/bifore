################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT EXTRACT GREYVALUES, FIRST DERIVATE & CALCULATED SD                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-08                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")

## Set filepath
path.lvl0300.csv <- "csv/kili/lvl0300_biodiversity_data_08022014.csv"


## Import dataset
data.raw <- read.csv2(path.lvl0300.csv,
                          dec = ",",
                          header = TRUE, 
                          stringsAsFactors = FALSE)

################################################################################
### Plot NA's in sd ############################################################

data.sd <- cbind(data.raw[3], data.raw[145:182])

## Create df, count NA's
data.sd.count.na <- data.frame(t(rbind(names(data.sd[2:39]),
                                     colSums(is.na(data.sd[2:39])))), row.names = NULL)

## Set colnames
colnames(data.sd.count.na) <- c("bands", "count NA")

## Plot NA's

