## Environmental stuff

# Working directory
setwd("/media/permanent/complementary_works/simon")

# Libraries
lib <- c("rgdal", "raster", "parallel")
lapply(lib, function(...) require(..., character.only = TRUE, quietly = TRUE))


## Data import

# Import shapefiles
shp.exp <- readOGR(dsn = "Daten/Exploratorien_shp", layer = "exploratorien_gebiet")
shp.dtl <- readOGR(dsn = "Daten/maps/src/verwaltungsgrenzen_deutschland", layer = "vg2500_sta")

# Plot shapefiles
plot(shp.dtl)
plot(shp.exp, add = TRUE)
text(coordinates(shp.exp), labels = as.character(shp.exp$Explorator), pos = 3)