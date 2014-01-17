################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK AND MODISCLOUD-PACKAGE  ##
##                                                                            ##
## Ref.: - http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf            ##
##       - MOD35 .hdf metadata                                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-06                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud" ,"doParallel")

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")

################################################################################
### Set filepaths ##############################################################

path.hdf.in <- ("/home/schmingo/SAVE/Diplomarbeit/myd03_myd35/")
path.hdf.sub <- ("/home/schmingo/SAVE/Diplomarbeit/sub_myd03_myd35/")


################################################################################
### Separate day and night hdf files ###########################################


fls.myd03 <- list.files(path.hdf.in,
                        pattern="MYD03",
                        full.names=TRUE)

fls.myd35 <- list.files(path.hdf.in,
                        pattern="MYD35",
                        full.names=TRUE)


## MYD03
registerDoParallel(cl <- makeCluster(4))

foreach(i in fls.myd03), .packages = lib) %dopar% {
  
  if (as.numeric(substr(basename(fls.myd03[i]),16,19)) > 1700)
    file.rename(from = fls.myd03[i],
                to = paste0(path.hdf.sub, basename(fls.myd03[i])))
}

stopCluster(cl)


## MYD35
registerDoParallel(cl <- makeCluster(4))

foreach(j in fls.myd35), .packages = lib) %dopar% {
  
  if (as.numeric(substr(basename(fls.myd35[j]),19,22)) > 1700)
    file.rename(from = fls.myd35[j],
                to = paste0(path.hdf.sub, basename(fls.myd35[j])))
}

  stopCluster(cl)