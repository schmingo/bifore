################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CREATE RANDOM ABUNDANCE DATA                                               ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-07                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))


## Set filepaths and filenames
path.wd <- "/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/"

path.abundance.out <- "csv/all_abundance.csv"
path.abundance.out.alb <- "csv/alb/alb_abundance.csv"
path.abundance.out.hai <- "csv/hai/hai_abundance.csv"
path.abundance.out.sch <- "csv/sch/sch_abundance.csv"


################################################################################
### Set working directory ######################################################

setwd(path.wd)

################################################################################
### Import MODIS data ##########################################################

data <- read.csv2("csv/MODIS_20130707-1120_RAW.csv",
                   dec = ".",
                   stringsAsFactors = FALSE)


################################################################################
### Subsetting #################################################################

data.sub <- data[,1:6]


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

## Write alb table
data.alb.abundance <- data.abundance[1:100,]
write.table(data.alb.abundance,
            file = path.abundance.out.alb, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")

## Write hai table
data.hai.abundance <- data.abundance[101:200,]
write.table(data.hai.abundance,
            file = path.abundance.out.hai, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")

## Write sch table
data.sch.abundance <- data.abundance[201:300,]
write.table(data.sch.abundance,
            file = path.abundance.out.sch, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")