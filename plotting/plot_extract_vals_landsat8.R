################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-27
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

## Plot greyvalues of a specific channel
scatter.ggplot <- ggplot(aes(x = Plotname, y = B1), data = table.grass)
g.sc <- scatter.ggplot + geom_point()
print(g.sc)

## Plot using qplot
qplot(Plotname, B01, data = table.grass, geom = "point")

## Plot using xyplot from 'latticeExtra' package
table.grass.heg01 <- subset(table.grass, Plotname == "HEG01")
xyplot(table.grass.heg01[, 2:ncol(table.grass.heg01)] ~ 
         as.factor(names(table.grass.heg01[2:ncol(table.grass.heg01)])), 
       type = "b", xlab = "Bands", ylab = "Grey value")

## ggplot try
str(table.grass.heg01)
print(colnames(table.grass.heg01))
print(as.factor(table.grass.heg01[1,2:13]))
print(table.grass.heg01)

ggplot(data = table.grass.heg01, aes(x = as.factor(names(table.grass.heg01[2:ncol(table.grass.heg01)])), 
                                     y = table.grass.heg01[1,2:13])) +  geom_point()

qplot(as.factor(names(table.grass.heg01[2:ncol(table.grass.heg01)])), table.grass.heg01[1,2:13], data = table.grass.heg01, geom = "point")