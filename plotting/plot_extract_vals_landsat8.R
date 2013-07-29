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
lib <- c("ggplot2", "latticeExtra", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Diplomarbeit/") # Linux
#setwd("D:/Diplomarbeit/") # Windows
# setwd("hier_kommt_der_Flo ;-)") # Linux
#setwd("E:/repositories/scripts") # Windows

## Import data 
table.all <- read.csv2("src/csv/hai_greyvalues_landsat8.csv", dec = ".",
                       header = TRUE, row.name = 1, stringsAsFactors = FALSE)


### Create Subsets

## different locations
table.grass <- subset(table.all, Location == "Grassland")
table.forest <- subset(table.all, Location == "Forest")

## delete redundant columns (coordinates, etc.)
table.grass <- table.grass[, -c(1:5)]
table.forest <- table.forest[, -c(1:5)]

## special exploratories-plots
table.grass.123 <- table.grass[c(1:3),]

## transpose dataframes
table.grass.trans <- data.frame(t(table.grass))
table.forest.trans <- data.frame(t(table.forest))
table.grass.123.trans <- data.frame(t(table.grass.123))

########################### Plotting ###########################################


### boxplot
## melt dataframes for boxplot
table.grass.trans.melt <- melt(table.grass.trans) # melt dataframes for boxplot
table.grass.123.trans.melt <- melt(table.grass.123.trans) # melt dataframes for boxplot

ggplot(data = table.grass.123.trans.melt, aes(x = variable, y = value))+ geom_boxplot()
ggplot(data = table.grass.trans.melt, aes(x = variable, y = value))+ geom_boxplot()


### scatterplot
ggplot(data = table.grass.trans, aes(rownames(table.grass.trans), HEG01)) + geom_point()

## scatterplot (qplot-version)
qplot(rownames(table.grass.trans),HEG01, data = table.grass.trans, geom=c("point"))

################################################################################
# ## Plot by Florian
# table.grass.heg01 <- subset(table.grass[1], Plotname == "HEG01")
# 
# xyplot(table.grass[1,2:ncol(table.grass)] ~ 
#       as.factor(names(table.grass[2:ncol(table.grass)])), 
#       type = "b", xlab = "Bands", ylab = "Grey value")