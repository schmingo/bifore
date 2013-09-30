################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOTTING EXTRACTED VALUES FROM LANDSAT8 SATELLITE DATA                     ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-09-30                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "latticeExtra", "reshape2", "RColorBrewer", "colorspace", "ggmap")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")


################################################################################
### Import data ################################################################

plot.center <- read.csv2("src/csv/hai/hai_plot_center.csv",
                        dec = ".",
                        header = TRUE, stringsAsFactors = FALSE)

data.ab <- read.csv2("src/csv/hai/hai_greyvalues_landsat8_abundance.csv",
                    dec = ".", 
                    header = TRUE, stringsAsFactors = FALSE)


################################################################################

data <- cbind(data.ab[,1:4], plot.center[,1:2], data.ab[7:ncol(data.ab)])

################################################################################
hainich.map <- qmap(location = 'Mülverstedt',
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


################################################################################

hainich2 <- get_map("Mülverstedt", zoom = 10)
hainich2
hainich2 +
  stat_density2d(
    aes(x = Longitude,
        y = Latitude,
        fill = ..level.., alpha = ..level..),
    size = 2, bins = 4,
    data = abundance,
    geom = "polygon")
