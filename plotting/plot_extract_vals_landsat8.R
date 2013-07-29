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
table.grass.123 <- table.grass[c(1:2),]

table.grass.trans <- data.frame(t(table.grass))
table.forest.trans <- data.frame(t(table.forest))
table.grass.123.trans <- data.frame(t(table.grass.123))

str(table.grass.trans)
################################################################################
######################### Plotting #############################################
################################################################################

## ggplot try
table.grass.melt <- melt(table.grass)
table.grass.trans.melt <- melt(table.grass.trans)
table.grass.123.trans.melt <- melt(table.grass.123.trans)

ggplot(data = table.grass.123.trans.melt, aes(x = variable, y = value)) + geom_boxplot()

ggplot(data = table.grass.trans, aes(rownames(table.grass.trans), HEG01)) + geom_point()
qplot(rownames(table.grass.trans),HEG01, data = table.grass.trans, geom=c("point"))

#Hints from Tim
# [11:40:15] Tim Appelhans: wieso alle? ggplot(data = ..., aes(x = kanal, y = value)) + geom_boxplot
# [11:40:17] Tim Appelhans: ()
# [11:40:30] Tim Appelhans: allerdings musst du den df vorher 'emlt(en)'
# [11:40:36] Tim Appelhans: sorry melt()
# [11:40:49] Tim Appelhans: heisst die funktion (package reshape)
# [11:41:03] Tim Appelhans: heisst du machst aus nem breiten atble nen langen table
# [11:41:42] Simon Schlauss: ist das sowas wie transpose t()?
# [11:41:50] Tim Appelhans: so ähnlich
# [11:42:25] Simon Schlauss: ok ich hab gewusst dass es noch am df liegt ;) ich probiere das mal aus
# [11:42:29] Tim Appelhans: macht aus deinen 12 variablen 2. eine mit dem factor level (kanal), eine mit den jeweiligen werten... is sowas wie das gegenteil von aggregate
# [11:42:55] Tim Appelhans: schau mal in die hilfe von melt, da sollte es einigermassen klar werden


## Plot by Florian
table.grass.heg01 <- subset(table.grass[1], Plotname == "HEG01")

xyplot(table.grass[1,2:ncol(table.grass)] ~ 
      as.factor(names(table.grass[2:ncol(table.grass)])), 
      type = "b", xlab = "Bands", ylab = "Grey value")