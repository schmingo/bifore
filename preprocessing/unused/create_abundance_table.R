################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CREATE RANDOM ABUNDANCE DATA                                               ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-31                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
# lib <- c()
# lapply(lib, function(...) require(..., character.only = TRUE))


## Set filepaths and filenames
path.wd <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/"

path.abundance.out <- "csv/kili/kili_abundance_dummy.csv"


################################################################################
### Set working directory ######################################################

setwd(path.wd)

################################################################################
### Import MODIS data ##########################################################

data <- read.csv2("csv/kili/MODIS_20060129_greyvalues_RAW.csv",
                   dec = ".",
                   stringsAsFactors = FALSE)


################################################################################
### Subsetting #################################################################

data.sub <- data[,1:5]


################################################################################
### create pseudo - abundance data #############################################

data.abundance <- cbind(data.sub,
                        abundance=sample(1:20,
                                         nrow(data.sub),
                                         replace = TRUE))


################################################################################
### Write abundance tables #####################################################

write.table(data.abundance, file = path.abundance.out, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")
