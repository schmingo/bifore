################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CONFIGURATION FILE                                                         ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-05-28                                                        ##
##                                                                            ##
################################################################################

## Windows
setwd("D:/Dropbox/Code/bifore/src/")
path.src <- "D:/Dropbox/Code/bifore/src/"
path.images <- "D:/Dropbox/Code/bifore/src/images/"
path.csv <- "D:/Dropbox/Code/bifore/src/csv/kili/"

file.varimp.MDA <- paste0(path.csv,"lvl0600_rf_prevalence_species-cut_mean100_MDA.csv")
file.varimp.MDG <- paste0(path.csv,"lvl0600_rf_prevalence_species-cut_mean100_MDG.csv")



## implementation example
# switch(Sys.info()[["sysname"]], 
#        "Windows" = source("D:/Code/bifore/config_filepaths_windows.R", echo=TRUE),
#        "Linux" = source("D:/Code/bifore/config_filepaths_linux.R", echo=TRUE))
