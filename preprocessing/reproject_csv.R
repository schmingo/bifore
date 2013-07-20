##################################
## REPROJECT CSV LATLONG TO UTM ##
##################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "sp")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") #Linux
setwd("D:/Diplomarbeit/") #Windows

## set filepaths
file.coords.alb <- "src/csv/alb_corner.csv"
file.coords.hai <- "src/csv/hai_corner.csv"
file.coords.sch <- "src/csv/sch_corner.csv"

## projection settings
input.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
output.proj <- "+proj=utm +zone=32 ellps=WGS84" # ToDo: check if it really works :)

## read data
coords.alb <- read.csv(file.coords.alb, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)
coords.hai <- read.csv(file.coords.hai, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)
coords.sch <- read.csv(file.coords.sch, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)

###############################################################################
###############################################################################
## ideas to continue
?project
# library(rgdal)
# xy <- cbind(c(118, 119), c(10, 50))
# project(xy, "+proj=utm +zone=51 ellps=WGS84")
# [,1]    [,2]
# [1,] -48636.65 1109577
# [2,] 213372.05 5546301

coordinates(coords.alb) <- c("Longitude", "Latitude")
#projection(coords.alb) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
tmp.tbl <- spTransform(coordinates(coords.alb), CRS("+proj=utm +datum=WGS84 +units=m")) #BUG!!!

###############################################################################
?spTransform

