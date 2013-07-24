################################################################################
## BiFoRe Scripts
##
## REPROJECT CSV LATLONG TO UTM
##
## Author: Simon Schlauss
## Version: 2013-07-23
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("rgdal", "sp", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
setwd("hier_kommt_der_Flo ;-)") # Linux
setwd("hier_kommt_der_Flo ;-)") # Windows

## Set filepath
file.coords <- "src/csv/all_plot_corner.csv"

## projection settings
input.proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
output.proj <- "+proj=utm +zone=32 ellps=WGS84 +units=m"

## Set output filename
out.name <- substr(basename(file.coords), 1, nchar(basename(file.coords)) - 4)
out.name <- paste("src/csv/", paste(out.name, "_utm", sep = ""), ".csv", sep = "")
print(out.name)

## read data
coords <- read.csv(file.coords, header = TRUE, sep = ";",dec = ".",
                      fill = FALSE, stringsAsFactors = FALSE)


### Reprojection

## Import coordinates as SpatialPointsDataframe
coordinates(coords) <- c("Longitude", "Latitude") 
#show(coordinates(coords))

## Set projection of imported data
projection(coords) <- input.proj

## Reproject coordinates
temp.table.utm <- spTransform(coords, CRS(output.proj))
show(temp.table.utm)

## Merge reprojected coordinates to csv
temp.dataframe <- data.frame(temp.table.utm) # create a dataframe
names(temp.dataframe)[c(6, 7)] <- c("utm_x", "utm_y") # rename coordinate columns (IMPORTANT: check number of columns!)
table.latlong.utm <- merge(data.frame(coords), temp.dataframe) # merge dataframes

## Write data to new csv
write.table(table.latlong.utm, file = out.name, dec = ".", quote = FALSE, 
            col.names = TRUE, row.names = FALSE, sep =";")
