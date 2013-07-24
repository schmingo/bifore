################################################################################
## BiFoRe Scripts
##
## PLOT SHAPEFILES
##
## Author: Simon Schlauss
## Version: 2013-07-23
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "parallel", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
setwd("hier_kommt_der_Flo ;-)") # Linux
setwd("hier_kommt_der_Flo ;-)") # Windows


### Data import

## Import shapefiles
shp.exp <- readOGR(dsn = "src/shapefiles/exploratories", layer = "exploratorien_gebiet")
shp.dtl <- readOGR(dsn = "src/shapefiles/verwaltungsgrenzen_deutschland", layer = "vg2500_sta")

## Plot shapefiles
plot(shp.dtl)
plot(shp.exp, add = TRUE)
text(coordinates(shp.exp), labels = as.character(shp.exp$Explorator), pos = 3)