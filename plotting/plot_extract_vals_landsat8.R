################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-07-28
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "latticeExtra", "reshape2", "RColorBrewer", "colorspace")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Diplomarbeit/") # Windows
# setwd("hier_kommt_der_Flo ;-)") # Linux
#setwd("E:/repositories/scripts") # Windows

## Import data 
all <- read.csv2("src/csv/hai_greyvalues_landsat8.csv", dec = ".",
                       header = TRUE, stringsAsFactors = FALSE)


### Create Subsets

## different locations
grass <- subset(all, Location == "Grassland")
forest <- subset(all, Location == "Forest")

## delete redundant columns (coordinates, etc.)
grass <- grass[, -c(2:6)]
forest <- forest[, -c(2:6)]

## special exploratories-plots
grass.123 <- grass[c(1:3),]

## transpose dataframes
# grass.t <- data.frame(t(grass))
# forest.t <- data.frame(t(forest))
# grass.123.t <- data.frame(t(grass.123))

########################### Plotting ###########################################


### boxplot
## melt dataframes for boxplot
grass.melt <- melt(grass, id = c("Plotname"), measured = c(grass[,2:nrow(grass)]))
forest.melt <- melt(forest, id = c("Plotname"), measured = c(forest[,2:nrow(forest)]))
grass.123.melt <- melt(grass.123, id = c("Plotname"), measured = c(grass.123[,2:nrow(grass.123)]))

## plot using ggplot2 package
ggplot(data = grass.melt, aes(x = variable, y = value))+ geom_boxplot()
ggplot(data = forest.melt, aes(x = variable, y = value))+ geom_boxplot()
ggplot(data = grass.123.melt, aes(x = variable, y = value, colour=Plotname)) + geom_point()


################################################################################
# ## Plot by Florian
# grass.heg01 <- subset(grass[1], Plotname == "HEG01")
# 
# xyplot(grass[1,2:ncol(grass)] ~ 
#       as.factor(names(grass[2:ncol(grass)])), 
#       type = "b", xlab = "Bands", ylab = "Grey value")