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

all.melt <- melt(all, 
                 id = c("Plotname", "Plotid","Status", "Location", "Longitude", "Latitude"),
                 measured = c(all[,7:nrow(all)]))


## plot using ggplot2 package
ggplot(data = grass.melt, aes(x = variable, y = value))+ geom_boxplot()
ggplot(data = forest.melt, aes(x = variable, y = value))+ geom_boxplot()
ggplot(data = grass.123.melt, aes(x = variable, y = value, colour=Plotname)) + geom_point()

ggplot(data = all.melt, aes(x = variable, y = value, colour=Location)) + geom_boxplot()


### lineplot
tmp <- melt(all[, c(1, 7:ncol(all))], id.vars = "Plotname", variable.name = "Band")
tmp <- tmp[order(tmp$Plotname), ]
ggplot(data = tmp, aes(x = Band, y = value, group = Plotname)) + 
  geom_line(aes(colour = Plotname))


### subset lineplot with facets
tmp2 <- subset(tmp, Plotname %in% c("HEG01", "HEG02", "HEG03", "HEG04", "HEG05"))
ggplot(data = tmp2, aes(x = Band, y = value, group = Plotname)) + 
  geom_point() + 
  geom_line(aes(colour = Plotname)) + 
  facet_wrap(~ Plotname, nrow = 2, ncol = 3)


# cbind(tmp2, sequence = seq(nrow(tmp2))) # generate column with seqence of nrow of tmp2
y <- cbind(tmp2, abundance=sample(1:20, nrow(tmp2), replace = TRUE)) # generate column with random Nr. between 1 and 20
################################################################################
# ## Plot by Florian
# grass.heg01 <- subset(grass[1], Plotname == "HEG01")
# 
# xyplot(grass[1,2:ncol(grass)] ~ 
#       as.factor(names(grass[2:ncol(grass)])), 
#       type = "b", xlab = "Bands", ylab = "Grey value")