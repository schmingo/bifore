################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-26
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "latticeExtra")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
# setwd("hier_kommt_der_Flo ;-)") # Linux
setwd("E:/repositories/scripts") # Windows

## Import files
table.all <- read.csv2("src/csv/hai_greyvalues_landsat8.csv", dec = ".", stringsAsFactors = FALSE)
table.all <- read.csv2("C:/Users/fdetsch/Downloads/hai_greyvalues_landsat8.csv", 
                       dec = ".", stringsAsFactors = FALSE)


## Create Subsets
table.grass <- subset(table.all, Location == "Grassland")
table.forest <- subset(table.all, Location == "Forest")

table.grass <- table.grass[, -c(2:6)] # deletes unused columns
table.forest <- table.forest[, -c(2:6)]

#df <- df[, c(1, 5:10)] # fügt bestimmt spalten zusammen
#df <- df[, -c(2:4)] # löscht bestimmte spalten

# Load and execute function to reformat colnames of input data
source("plotting/reformatColnames.R")

table.grass <- reformatColnames(table.grass)
table.forest <- reformatColnames(table.forest)


## Plot greyvalues of a specific channel
scatter.ggplot <- ggplot(aes(x = Plotname, y = B1), data = table.grass)
g.sc <- scatter.ggplot + geom_point()
print(g.sc)

## Plot using qplot
qplot(Plotname, B1, data = table.grass, geom = "point")

## Plot using xyplot from 'latticeExtra' package
table.grass.heg01 <- subset(table.grass, Plotname == "HEG01")
xyplot(table.grass.heg01[, 2:ncol(table.grass.heg01)] ~ 
         as.factor(names(table.grass.heg01[2:ncol(table.grass.heg01)])), 
       type = "b", xlab = "Bands", ylab = "Grey value")
