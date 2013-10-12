################################################################################
## BiFoRe Scripts
##
## PLOTTING EXTRACTED VALUES FROM LANDSAT8 SATELLITE DATA
##
## Author: Simon Schlauss (sschlauss@gmail.com)
## Version: 2013-10-11
##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "latticeExtra", "reshape2", "RColorBrewer", "colorspace")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
#setwd("/home/schmingo/Diplomarbeit/") # Linux
setwd("D:/Dropbox/Diplomarbeit/code/bifore/") # Windows
#setwd("Florian")

## Import data 
data <- read.csv2("src/csv/MODIS_20130706-1040_greyvalues_NA.csv", dec = ".",
                       header = TRUE, stringsAsFactors = FALSE)


### Create Subsets

## different locations
grass <- subset(data, Location == "Grassland")
forest <- subset(data, Location == "Forest")

## delete redundant columns (coordinates, etc.)
grass <- grass[, -c(2:3)]
forest <- forest[, -c(2:3)]

## special exploratories-plots
grass.select <- grass[c(1:8),]

## transpose dataframes
# grass.t <- data.frame(t(grass))
# forest.t <- data.frame(t(forest))
# grass.select.t <- data.frame(t(grass.select))

########################### Plotting ###########################################


### boxplot
## melt dataframes for boxplot
grass.melt <- melt(grass, id = c("Plotname","Location", "Longitude", "Latitude"), measured = c(grass[,5:nrow(grass)]))
forest.melt <- melt(forest, id = c("Plotname","Location", "Longitude", "Latitude"), measured = c(forest[,5:nrow(forest)]))
grass.select.melt <- melt(grass.select, id = c("Plotname","Location", "Longitude", "Latitude"), measured = c(grass.select[,5:nrow(grass.select)]))

data.melt <- melt(data, 
                 id = c("Plotname", "Plotid","Status", "Location", "Longitude", "Latitude"),
                 measured = c(data[,7:nrow(data)]))


## plot using ggplot2 package
ggplot(data = grass.melt, aes(x = variable, y = value))+ geom_boxplot()

ggplot(data = forest.melt, aes(x = variable, y = value))+ geom_boxplot()

ggplot(data = forest.melt, aes(x = variable, y = value, group=1))+ geom_smooth()

ggplot(data = grass.select.melt, aes(x = variable, y = value, colour=Plotname)) + 
  geom_point()

ggplot(data = data.melt, aes(x = variable, y = value, colour=Location)) + 
  geom_boxplot()


### lineplot
tmp.line <- melt(data[, c(1, 7:ncol(data))], id.vars = "Plotname", variable.name = "Band")
tmp.line <- tmp.line[order(tmp.line$Plotname), ]

ggplot(data = tmp.line, aes(x = Band, y = value, group = Plotname)) + 
  geom_line(aes(colour = Plotname))


### subset lineplot with facets
tmp2.line <- subset(tmp.line, Plotname %in% c("HEG01", "HEG02", "HEG03", "HEW01", "HEW02", "HEW03"))
ggplot(data = tmp2.line, aes(x = Band, y = value, group = Plotname)) + 
  geom_point() + 
  geom_line(aes(colour = Plotname)) + 
  facet_wrap(~ Plotname, nrow = 2, ncol = 3)


################################################################################
# ## Plot by Florian
# grass.heg01 <- subset(grass[1], Plotname == "HEG01")
# 
# xyplot(grass[1,2:ncol(grass)] ~ 
#       as.factor(names(grass[2:ncol(grass)])), 
#       type = "b", xlab = "Bands", ylab = "Grey value")