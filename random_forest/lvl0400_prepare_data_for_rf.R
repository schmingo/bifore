################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## DATA PREPARATION FOR RANDOM FOREST                                         ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-11                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Import dataset
data <- read.csv2("csv/kili/lvl0300_biodiversity_data_2014-02-12.csv",
                    dec = ",",
                    header = TRUE,
                    stringsAsFactors = FALSE)


################################################################################
### Combining and subsetting data ##############################################

data.raw <- data.frame(data)
tmp.speciesnr <- data.raw[9]

### Biodiversity data
## Modify biodiversity values to 2 digit numeric value
tmp.speciesnr.list <- as.list(as.numeric(t(tmp.speciesnr)))
tmp.speciesnr.list <- formatC(tmp.speciesnr.list, 
                              width = 2, 
                              format = "d", 
                              flag = "0")

## Modify biodiversity values: paste "SP" in front to create a character
tmp.speciesnr <- as.data.frame(paste0("SP", tmp.speciesnr.list))

names(tmp.speciesnr) <- "SpeciesNr"


### Observation date and corresponding greyvalues

data.greyval <- cbind(data.raw[2], data.raw[69:106])
data.diff <- cbind(data.raw[2], data.raw[107:144])
data.sd <- cbind(data.raw[2], data.raw[145:182])


data.greyval.na <- data.frame(colSums(is.na(data.greyval)))
names(data.greyval.na) <- c("NAs out of 225")


data.diff.na <- data.frame(colSums(is.na(data.diff)))
names(data.diff.na) <- c("NAs out of 225")

data.sd.na <- data.frame(colSums(is.na(data.sd)))
names(data.sd.na) <- c("NAs out of 225")

