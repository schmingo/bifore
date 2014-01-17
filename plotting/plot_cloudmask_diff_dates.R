################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT CLOUDMASK DIFF DATES                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-06                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2")

lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")

path.nocloud.csv <- ("csv/kili/cloudcheck_diff_dates.csv")

################################################################################
### Set filepaths ##############################################################

data <- read.csv2(path.nocloud.csv,
                  dec = ".",
                  header = TRUE, 
                  stringsAsFactors = TRUE)



# qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, facets=, xlim=, ylim= xlab=, ylab=, main=, sub=) 

qplot(x=diff_days,
      data=data,
      geom="histogram",
      binwidth=0.5)
