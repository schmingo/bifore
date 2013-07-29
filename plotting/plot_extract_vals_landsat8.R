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
#setwd("D:/Diplomarbeit/") # Windows
# setwd("hier_kommt_der_Flo ;-)") # Linux
#setwd("E:/repositories/scripts") # Windows

## Import data 
table.all <- read.csv2("src/csv/hai_greyvalues_landsat8.csv", dec = ".",
                       header = TRUE, stringsAsFactors = FALSE)
#table.all <- read.csv2("C:/Users/fdetsch/Downloads/hai_greyvalues_landsat8.csv", dec = ".", stringsAsFactors = FALSE)
str(table.all)

## Transpose data
#table.all.trans <- data.frame(t(table.all))
#str(table.all.trans)

## Create Subsets
table.grass <- subset(table.all, Location == "Grassland")
table.forest <- subset(table.all, Location == "Forest")

table.grass <- table.grass[, -c(2:6)] # deletes unused columns
table.forest <- table.forest[, -c(2:6)]

table.grass.trans <- data.frame(t(table.grass))
table.forest.trans <- data.frame(t(table.forest))


################################################################################
######################### Plotting #############################################
################################################################################
## Plot greyvalues of a specific channel
# scatter.ggplot <- ggplot(aes(x = Plotname, y = B1), data = table.grass)
# g.sc <- scatter.ggplot + geom_point()
# print(g.sc)

## Plot using qplot
# qplot(Plotname, B01, data = table.grass, geom = "point")

## Plot using xyplot from 'latticeExtra' package
table.grass.heg01 <- subset(table.grass[1] Plotname == "HEG01")
xyplot(table.grass.heg01[, 2:ncol(table.grass.heg01)] ~ 
      as.factor(names(table.grass.heg01[2:ncol(table.grass.heg01)])), 
      type = "b", xlab = "Bands", ylab = "Grey value")

## ggplot try
################################################################################

str(table.grass)

ggplot(table.grass, aes(colnames(table.grass),table.grass[1])) + geom_bar()

data(diamonds)
str(diamonds)



################################################################################
hefeweizen <- ggplot(data = table.grass, aes(x = as.factor(names(table.grass[2:ncol(table.grass)])), 
                                             y = table.grass[1,2:ncol(table.grass)]) +  geom_point()
                     
qplot(as.factor(colnames(table.grass.heg01[2:ncol(table.grass.heg01)])), 
      table.grass.heg01[1,2:ncol(table.grass.heg01)], data = table.grass.heg01, 
      geom = "bar")