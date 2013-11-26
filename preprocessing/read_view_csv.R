################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Import and view csv files                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-11-26                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))


## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/")

## set filepaths
file.abundance.csv <- "src/csv/kili/abundance_matrix_hemp.csv"


################################################################################
## read data
data <- read.csv2(file.abundance.csv, 
                     header = TRUE, 
                     sep = ";",
                     dec = ".",
                     fill = FALSE, 
                     stringsAsFactors = FALSE)


# data$date <- as.POSIXct(data$date, format="%m/%d/%Y")
data$date <- as.Date(data$date, format="%m/%d/%Y")

################################################################################
## replace 0-values with NA

# subset data
data.species <- data[,9:ncol(data)]

# set 0-values to NA
data.species[data.species==0] <- NA

# recombine data
data[,9:ncol(data)] <- data.species


################################################################################
## plot single species
plot(data[,10],data$date, type="p")

## remove observations before MODIS satellite launch
##      Note: MODIS TERRA launch: 1999-12-18
##            MODIS AQUA launch: 2002-05-04

## ggplot2 single species
species <- geom_point(aes(x = data[,9],
                          y = data[,2],
                          colour = "black"),
                      data = data)
modis.date <- as.Date("2000-01-01")
data <- subset(data, date > modis.date)

speciesplot <- ggplot() + species +
  xlab("Species") +
  ylab("Abundance") +
  ggtitle("Species - Abundance")

speciesplot