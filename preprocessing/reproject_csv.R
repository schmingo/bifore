##################################
## REPROJECT CSV LATLONG TO UTM ##
##################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "sp", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/scripts/") #Linux
setwd("D:/Diplomarbeit/scripts/") #Windows
setwd("hier_kommt_der_Flo ;-)")

## set filepaths
# file.coords.alb <- "csv/alb_corner.csv"
file.coords.hai <- "csv/hai_corner.csv"
#file.coords.sch <- "csv/sch_corner.csv"

## projection settings
input.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
output.proj <- "+proj=utm +zone=32 ellps=WGS84 +units=m"

## read data
# coords.alb <- read.csv(file.coords.alb, header = TRUE, sep = ";",dec = ".",
#                        fill = FALSE, stringsAsFactors = FALSE)
coords.hai <- read.csv(file.coords.hai, header = TRUE, sep = ";",dec = ".",
                      fill = FALSE, stringsAsFactors = FALSE)
#coords.sch <- read.csv(file.coords.sch, header = TRUE, sep = ";",dec = ".",
#                       fill = FALSE, stringsAsFactors = FALSE)

###############################################################################
###############################################################################

### Reprojection

## Import coordinates as SpatialPointsDataframe
coordinates(coords.hai) <- c("Longitude", "Latitude") 
#show(coordinates(coords.hai))

## Set projection of imported data
projection(coords.hai) <-  input.proj

## Reproject coordinates
temp.table.utm <- spTransform(coords.hai, CRS(output.proj))
show(temp.table.utm)

## Write projected coordinates to csv
temp.dataframe <- data.frame(temp.table.utm) # create a dataframe
names(temp.dataframe)[c(6, 7)] <- c("utm_x", "utm_y") # rename coordinate columns
table.latlong.utm <- merge(data.frame(coords.hai), temp.dataframe) # merge data
