##################################
## REPROJECT CSV LATLONG TO UTM ##
##################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "sp", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Diplomarbeit/") #Linux
setwd("E:/repositories/scripts/") #Windows

## set filepaths
# file.coords.alb <- "src/csv/alb_corner.csv"
file.coords.hai <- "E:/complementary_works/simon/Daten/hai_corner.csv"
#file.coords.sch <- "src/csv/sch_corner.csv"

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
## ideas to continue
#?spTransform
#?project
# library(rgdal)
# xy <- cbind(c(118, 119), c(10, 50))
# project(xy, "+proj=utm +zone=51 ellps=WGS84")
# [,1]    [,2]
# [1,] -48636.65 1109577
# [2,] 213372.05 5546301
###############################################################################
###############################################################################

### Reprojection

## Import coordinates as SpatialPointsDataframe
coordinates(coords.hai) <- c("Longitude", "Latitude") 
#show(coordinates(coords.hai))

## Set projection of imported data
projection(coords.hai) <-  input.proj

## Version 1 using 'spTransform'
temp.table.utm <- spTransform(coords.hai, CRS(output.proj)) #BUG!!!
show(temp.table.utm)

## Version 2 using 'project'
# temp.table.utm <- project(coordinates(coords.alb), output.proj) #seems to work!
#show(temp.table.utm)
# Note: you cannot reproject a spatial object with an unknown CRS!

## NEXT STEP -> write projected coordinates to csv
temp.dataframe <- data.frame(temp.table.utm)
names(temp.dataframe)[c(6, 7)] <- c("x", "y") # rename coordinate columns
table.latlong.utm <- merge(data.frame(coords.hai), temp.dataframe) # 
