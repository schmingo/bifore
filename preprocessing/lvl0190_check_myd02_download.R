################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Compare downloaded MYD02 with nocloud dates                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-24                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("d:/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

path.biodiversity.csv <- ("csv/kili/lvl0100_biodiversity_data.csv")

# path.hdf.in <- ("E:/Diplomarbeit/myd02_hdf/")
path.hdf.in <- ("/home/schmingo/Diplomarbeit/myd02_hdf/")

################################################################################
### Import biodiversity dataset ################################################

data <- read.csv2(path.biodiversity.csv,
                  dec = ".",
                  header = TRUE, 
                  stringsAsFactors = FALSE)


################################################################################
### Extract MYD02 date string ##################################################

## List hdf files
lst.1km <- list.files(path.hdf.in,
                      pattern="MYD021KM",
                      full.names=TRUE)
  
lst.qkm <- list.files(path.hdf.in,
                      pattern="MYD02QKM",
                      full.names=TRUE)

lst.hkm <- list.files(path.hdf.in,
                      pattern="MYD02HKM",
                      full.names=TRUE)


## Extract date string from filename
dates.1km <- substr(basename(lst.1km), 11, 22)
dates.1km <- paste0(substr(dates.1km, 0, 4),
                    "-",
                    substr(dates.1km, 5, 7),
                    "_",
                    substr(dates.1km, 9, 13))

dates.qkm <- substr(basename(lst.qkm), 11, 22)
dates.qkm <- paste0(substr(dates.qkm, 0, 4),
                    "-",
                    substr(dates.qkm, 5, 7),
                    "_",
                    substr(dates.qkm, 9, 13))

dates.hkm <- substr(basename(lst.hkm), 11, 22)
dates.hkm <- paste0(substr(dates.hkm, 0, 4),
                    "-",
                    substr(dates.hkm, 5, 7),
                    "_",
                    substr(dates.hkm, 9, 13))


dates.1km
dates.qkm
dates.hkm


################################################################################
### Check if downloaded MYD02 files are complete ###############################

data$date_nocloud %in% dates.1km
data$date_nocloud %in% dates.qkm
data$date_nocloud %in% dates.hkm

dates.1km %in% data$date_nocloud
dates.qkm %in% data$date_nocloud
dates.hkm %in% data$date_nocloud

