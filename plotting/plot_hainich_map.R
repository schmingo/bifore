################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOTTING EXTRACTED RAW VALUES FROM LANDSAT8 SATELLITE DATA                 ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-10-02                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "ggmap")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import data ################################################################

plot.center <- read.csv2("csv/hai/hai_plot_center.csv",
                        dec = ".",
                        header = TRUE, stringsAsFactors = FALSE)

data.ab <- read.csv2("csv/hai/hai_greyvalues_landsat8_abundance.csv",
                    dec = ".", 
                    header = TRUE, stringsAsFactors = FALSE)


################################################################################
### Get geocoordinates from plot center ########################################

data <- cbind(data.ab[,1:4], plot.center[,1:2], data.ab[7:ncol(data.ab)])


################################################################################
### Plot Hainich - Abundance map ###############################################

## Define output image | open image port
png("images/map_hainich_abundance.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

## Plot
hainich.map <- qmap(location = "Muelverstedt",
                    color = "bw",
                    legend = "bottomright",
                    extent = "device",
                    maptype = "terrain")
hainich.map

points <- geom_point(
                     aes(x = Longitude,
                         y = Latitude,
                         colour = abundance,
                         fill = abundance,
                         size = 2),
                     data = data)

hainich.map + points

## Close image port
graphics.off()
