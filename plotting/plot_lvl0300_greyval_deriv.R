################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOT EXTRACT GREYVALUES, FIRST DERIVATE                                    ##
##                                                                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-11                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))


## Required libraries
lib <- c("ggplot2", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Set filepath
path.lvl0300.csv <- "csv/kili/lvl0300_biodiversity_data.csv"
path.bandnames.csv <- "csv/kili/bandnames.csv"


## Import dataset
data.raw <- read.csv2(path.lvl0300.csv,
                      dec = ",",
                      header = TRUE, 
                      stringsAsFactors = FALSE)

bandnames <- read.csv2(path.bandnames.csv,
                       header = TRUE,
                       stringsAsFactors = FALSE)

################################################################################
### Subsetting #################################################################
################################################################################

## greyvalues reflective bands 
data.greyval.reflective <- cbind.data.frame(data.raw[,3],
                                            data.raw[,69:89],
                                            data.raw[,96])
names(data.greyval.reflective) <- c("date_nocloud",
                                    as.character(bandnames[1:21,2]),
                                    as.character(bandnames[28,2]))
names(data.greyval.reflective)

greyval.reflective.melt <- melt(data.greyval.reflective, id = "date_nocloud", 
                                measured = c(data.greyval.reflective[,2:ncol(data.greyval.reflective)]))


## greyvalues emissive bands
data.greyval.emissive <- cbind.data.frame(data.raw[,3],
                                          data.raw[,90:95],
                                          data.raw[,97:106])
names(data.greyval.emissive) <- c("date_nocloud",
                                  as.character(bandnames[22:27,2]),
                                  as.character(bandnames[29:38,2]))
names(data.greyval.emissive)

greyval.emissive.melt <- melt(data.greyval.emissive, id = "date_nocloud", 
                              measured = c(data.greyval.emissive[,2:ncol(data.greyval.emissive)]))



################################################################################
### Plotting ###################################################################
################################################################################

plot.greyval.reflective <- ggplot(greyval.reflective.melt, aes(x = variable, y = value)) +
  geom_boxplot()
plot.greyval.reflective



plot.greyval.emissive <- ggplot(greyval.emissive.melt, aes(x = variable, y = value)) +
  geom_boxplot()
plot.greyval.emissive
