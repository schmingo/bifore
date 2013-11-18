################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Import and view csv files                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-11-18                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries


## set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")

## set filepaths
file.abundance <- "src/csv/kili/data matrix hemp.csv"


## set working directory
#setwd(D:/Dropbox/Diplomarbeit/Daten&read_view/)

## read data
data <- read.csv(file.abundance, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)


## show data
str(data)

summary(data)