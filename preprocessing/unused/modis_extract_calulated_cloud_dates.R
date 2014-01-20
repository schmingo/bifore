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
lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("d:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

path.nocloud.csv <- ("csv/kili/biodiversity_data_cloudchecked.csv")
path.nocloud.out.csv <- ("csv/kili/biodiversity_data_cloudchecked_sub.csv")

################################################################################
### Import biodiversity dataset ################################################

data <- read.csv2(path.nocloud.csv,
#                   dec = ".",
                  header = TRUE, 
                  stringsAsFactors = FALSE)

data.cloud <- data[c(2,4,3)]

str(data.cloud)

data.cloud <- transform(data.cloud, date_nocloud = paste0(substr(data.cloud$date_nocloud, 1,4), 
                                                   "-", 
                                                   substr(data.cloud$date_nocloud, 5, 7), 
                                                   "_", 
                                                   substr(data.cloud$date_nocloud, 9, 12)))

data.cloud <- transform(data.cloud, date_observation = paste0(substr(data.cloud$date_observation, 1,4), 
                                                              "-", 
                                                              substr(data.cloud$date_observation, 5, 7)))

write.table(data.cloud, 
            file = path.nocloud.out.csv,
            dec = ".",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
