################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Plot Kilimanjaro maps                                                      ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-04                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "raster", "sp", "ggmap", "ggsubplot")
lapply(lib, function(...) require(..., character.only = TRUE))


## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## set filepaths
file.biodiversity.csv <- "csv/kili/lvl0050_biodiversity_data.csv"

################################################################################
## read dataset ################################################################
################################################################################

data.raw <- read.csv2(file.biodiversity.csv, 
                      header = TRUE, 
                      sep = ";",
                      dec = ".",
                      fill = FALSE, 
                      stringsAsFactors = FALSE)

################################################################################
## sp plot #####################################################################
################################################################################

data.raw.sp <- data.raw

coordinates(data.raw.sp) <- ~coordW + coordN
spplot(data.raw.sp, zcol = "asl")

################################################################################
### ggmap ######################################################################
################################################################################

## Define output image | open image port
# png("images/map_kili_overview.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

## Plot
kili.map <- qmap(location = "Kilimanjaro",
                 color = "color",
                 legend = "bottomright",
                 extent = "panel",
                 maptype = "terrain")

points <- geom_point(
  aes(x = coordW,
      y = coordN,
      #       colour = asl,
      #       fill = abundance,
      size = 3),
  data = data.raw)

map <- kili.map # + points
map

# graphics.off()
