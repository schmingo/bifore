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
lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

path.speciesNR <- "csv/kili/lvl0400_speciesNR.csv"
path.NA <- "csv/kili/lvl0400_NAsummary.csv"

## Import dataset
data <- read.csv2("csv/kili/lvl0300_biodiversity_data.csv",
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


## Observation date and corresponding greyvalues
data.greyval <- cbind(data.raw[2], data.raw[69:106])
data.diff <- cbind(data.raw[2], data.raw[107:144])
data.sd <- cbind(data.raw[2], data.raw[145:182])

## Create NA tables
data.greyval.na <- data.frame(colSums(is.na(data.greyval[,2:ncol(data.greyval)])))
names(data.greyval.na) <- c("NAs out of 225")

data.diff.na <- data.frame(colSums(is.na(data.diff[,2:ncol(data.greyval)])))
names(data.diff.na) <- c("NAs out of 225")

data.sd.na <- data.frame(colSums(is.na(data.sd[,2:ncol(data.greyval)])))
names(data.sd.na) <- c("NAs out of 225")

################################################################################
### Create final df for RandomForest ###########################################

data.speciesNR <- data.frame(cbind(data.raw[1:3], 
                                   data.raw[69:78], data.raw[87:106],     ## greyvalues
                                   data.raw[108:116], data.raw[126:144],  ## diff
                                   data.raw[145:154], data.raw[163:182],  ## sd
                                   tmp.speciesnr))
names(data.speciesNR)
ifelse(TRUE %in% is.na(data.speciesNR), print("NA Werte enthalten"), print("Alles Ok!"))
write.table(data.speciesNR, 
            file = path.speciesNR,
            dec = ".",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


################################################################################
### Write NA summary csv #######################################################

## Combine NA tables
na.tables <- data.frame(cbind("bands" = c(1:12, 13.1, 13.2, 14.1, 14.2, 15:36),
                              data.greyval.na,
                              data.diff.na,
                              data.sd.na))
names(na.tables) <- c("bands", "NAs greyval", "NAs diff", "NAs sd")

## Write table
write.table(na.tables, 
            file = path.NA, 
            quote = FALSE, 
            sep = ";", 
            dec = ",", 
            row.names = FALSE, 
            col.names = TRUE) 