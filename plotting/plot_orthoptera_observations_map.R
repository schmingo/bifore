################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOTTING ORTHOPTERA OBSERVATIONS AT MT. KILIMANJARO                        ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-12-02                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "ggmap", "raster", "sp", "rgdal")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
# setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import data ################################################################

data <- read.csv2("csv/kili/abundance_data_subset.csv",
                     dec = ".",
                     header = TRUE, 
                     stringsAsFactors = TRUE,
                     )

data.sp <- data
################################################################################
### Transform UTM to LatLong Coordinates #######################################

coordinates(data.sp) <- c("coordW", "coordN")
projection(data.sp) <- "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
data.sp <- spTransform(data.sp, CRS("+proj=longlat"))

data.sp <- as.data.frame(data.sp)

data.sp <- cbind(data[1:3], 
                 data.sp$coordW, 
                 data.sp$coordN,
                 data[6:ncol(data)])

names(data.sp)[4] <- "lon"
names(data.sp)[5] <- "lat"

data <- data.sp

data.species <- data[,1:9]

################################################################################
### Plots ######################################################################

kili.extent <- get_map(location = c(36.93865,
                                    -3.454621,
                                    37.76235,
                                    -2.775392),
#                        zoom = 11,
                       scale = "auto",
                       maptype = "satellite",
                       color = "bw",
                       source = "google")

kilimap <- ggmap(kili.extent, 
                 extent = "normal",
                 maprange = TRUE)


orthoptera.obs1 <- geom_jitter(aes(x = lon,
                                  y = lat,
                                  colour = nr.of.species,
                                  size = 10),
                              show_guide = FALSE,
                              data = data.species)

colourscale <- scale_colour_gradient(low = "white", 
                                     high = "darkgreen", 
                                     name = "Number of species"
                                     )


kilimap + orthoptera.obs1 + colourscale