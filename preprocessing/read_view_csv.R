################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Import and view csv files                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-11-26                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))


## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/")

## set filepaths
file.abundance.csv <- "src/csv/kili/abundance_matrix_hemp.csv"


################################################################################
## read data
data <- read.csv2(file.abundance.csv, 
                     header = TRUE, 
                     sep = ";",
                     dec = ".",
                     fill = FALSE, 
                     stringsAsFactors = FALSE)


# data$date <- as.POSIXct(data$date, format="%m/%d/%Y")
data$date <- as.Date(data$date, format="%m/%d/%Y")

################################################################################