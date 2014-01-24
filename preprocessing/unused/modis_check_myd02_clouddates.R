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
lib <- c("MODIS")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("d:/Dropbox/Diplomarbeit/code/bifore/src/")
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

path.nocloud.csv <- ("csv/kili/biodiversity_data_cloudchecked.csv")
path.nocloud.out.csv <- ("csv/kili/biodiversity_data_cloudchecked_sub.csv")

path.hdf.in <- ("E:/Diplomarbeit/myd02_hdf/")

################################################################################
### Import biodiversity dataset ################################################

data <- read.csv2(path.nocloud.out.csv,
#                   dec = ".",
                  header = TRUE, 
                  stringsAsFactors = FALSE)

# data$date_nocloud <- as.Date(data$date_nocloud, format("%Y-%j_%H%M"))


################################################################################
### List check downloaded MYD02 files ##########################################

### List hdf files
lst.1km <- list.files(path.hdf.in,
                      pattern="MYD021KM",
                      full.names=TRUE)
  
lst.qkm <- list.files(path.hdf.in,
                      pattern="MYD02QKM",
                      full.names=TRUE)

lst.hkm <- list.files(path.hdf.in,
                      pattern="MYD02HKM",
                      full.names=TRUE)



tmp.dates <- substr(basename(lst.1km), 11, 22)
myd02.dates <- paste0(substr(tmp.dates, 0, 4),
                      "-",
                      substr(tmp.dates, 5, 7),
                      "_",
                      substr(tmp.dates, 9, 13))


myd02.dates
##########################

data$date_nocloud %in% myd02.dates
data$date_nocloud[10] %in% myd02.dates

data$date_nocloud[10]
