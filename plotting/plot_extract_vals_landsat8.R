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
                       header = TRUE, row.name = 1, stringsAsFactors = FALSE)
#table.all <- read.csv2("C:/Users/fdetsch/Downloads/hai_greyvalues_landsat8.csv", dec = ".", stringsAsFactors = FALSE)
str(table.all)

## Transpose data
#table.all.trans <- data.frame(t(table.all))
#str(table.all.trans)

## Create Subsets
table.grass <- subset(table.all, Location == "Grassland")
table.forest <- subset(table.all, Location == "Forest")

table.grass <- table.grass[, -c(1:5)] # deletes unused columns
table.forest <- table.forest[, -c(1:5)]

table.grass.trans <- data.frame(t(table.grass))
table.forest.trans <- data.frame(t(table.forest))
rownames(table.grass.trans)

str(table.grass.trans)
summary(table.grass.trans)
print(row.names(table.grass.trans))

################################################################################
######################### Plotting #############################################
################################################################################

## ggplot try
ggplot(table.grass.trans, aes(rownames(table.grass.trans), HEG01)) + geom_bar()




## Plot using xyplot from 'latticeExtra' package
table.grass.heg01 <- subset(table.grass[1] Plotname == "HEG01")

xyplot(table.grass.heg01[, 2:ncol(table.grass.heg01)] ~ 
      as.factor(names(table.grass.heg01[2:ncol(table.grass.heg01)])), 
      type = "b", xlab = "Bands", ylab = "Grey value")