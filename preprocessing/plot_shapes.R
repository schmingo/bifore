## Environmental stuff

## Clear workspace
rm(list = ls(all = TRUE))

# Working directory
setwd("/home/schmingo/Diplomarbeit/") #Linux
setwd("D:/Diplomarbeit/") #Windows

# Libraries
lib <- c("rgdal", "raster", "parallel")
lapply(lib, function(...) require(..., character.only = TRUE, quietly = TRUE))


## Data import

# Import shapefiles
shp.exp <- readOGR(dsn = "src/shapefiles/exploratories", layer = "exploratorien_gebiet")
shp.dtl <- readOGR(dsn = "src/shapefiles/verwaltungsgrenzen_deutschland", layer = "vg2500_sta")

# Plot shapefiles
plot(shp.dtl)
plot(shp.exp, add = TRUE)
text(coordinates(shp.exp), labels = as.character(shp.exp$Explorator), pos = 3)