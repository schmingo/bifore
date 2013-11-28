################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Plot Kilimanjaro Maps                                                      ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-11-28                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "raster", "sp", "ggmap", "ggsubplot")
lapply(lib, function(...) require(..., character.only = TRUE))


## set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")
# setwd("D:/Dropbox/Diplomarbeit/code/bifore/")

## set filepaths
file.abundance.csv <- "src/csv/kili/abundance_data_subset.csv"

################################################################################
## read dataset

data10 <- read.csv2(file.abundance.csv, 
                  header = TRUE, 
                  sep = ";",
                  dec = ".",
                  fill = FALSE, 
                  stringsAsFactors = FALSE)

################################################################################
## sp plot
data10.sp <- data10

coordinates(data10.sp) <- ~coordW + coordN
spplot(data10.sp, zcol = "asl")


### ggmap

## Define output image | open image port
png("images/map_kili_abundance.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

## Plot
kili.map <- qmap(location = "Kilimanjaro",
                    color = "color",
                    legend = "bottomright",
                    extent = "panel",
                    maptype = "terrain")

points <- geom_point(
  aes(x = coordW,
      y = coordN,
      colour = asl,
      #                          fill = abundance,
      size = 3),
  data = data10)

map <- kili.map + points

graphics.off()