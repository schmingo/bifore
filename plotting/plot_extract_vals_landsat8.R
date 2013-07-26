################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-25
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
#setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
#setwd("hier_kommt_der_Flo ;-)") # Linux
#setwd("hier_kommt_der_Flo ;-)") # Windows

## Import files
table.all <- read.csv2("src/csv/hai_greyvalues_landsat8.csv", dec = ".", stringsAsFactors = FALSE)


## Create Subsets
table.grass <- subset(table.all, Location == "Grassland")
table.forest <- subset(table.all, Location == "Forest")

table.grass <- table.grass[, -c(2:6)] # deletes unused columns
table.forest <- table.forest[, -c(2:6)]

#df <- df[, c(1, 5:10)] # fügt bestimmt spalten zusammen
#df <- df[, -c(2:4)] # löscht bestimmte spalten

## Plot greyvalues of a specific channel
scatter.ggplot <- ggplot(aes(x = Plotname, y = B1), data = table.grass)
g.sc <- scatter.ggplot + geom_point()
print(g.sc)