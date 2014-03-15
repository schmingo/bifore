################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PREPARE BIODIVERSITY TABLE                                                 ##
##                                                                            ##
## - Replace 0-values with NA                                                 ##
## - Remove observations before MODIS launch date                             ##
## - Remove observations without coordinates                                  ##
## - Remove species with less than 10 observations in different plots         ##
## - Calculate number of species                                              ##
## - Add LatLong Coordinates                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-04                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "raster", "rgdal")
lapply(lib, function(...) require(..., character.only = TRUE))


## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Set filepaths
file.hemp.csv <- "csv/kili/abundance_matrix_hemp.csv"
file.data.out <- "csv/kili/lvl0050_biodiversity_data_all_spec.csv"
file.prevalence.out <- "csv/kili/lvl0050_prevalence_all_spec.csv"


################################################################################
### Read data ##################################################################
################################################################################

data <- read.csv2(file.hemp.csv, 
                  header = TRUE, 
                  sep = ";",
                  dec = ".",
                  fill = FALSE, 
                  stringsAsFactors = FALSE)


data$date <- as.Date(data$date, format="%d.%m.%Y")


################################################################################
### Replace 0-values with NA ###################################################
################################################################################

## Subset data
data.species <- data[,9:ncol(data)]

## Set 0-values to NA
data.species[data.species==0] <- NA

## Recombine data
data[,9:ncol(data)] <- data.species


################################################################################
### Remove observations before MODIS satellite launch ##########################
################################################################################

##      Note: MODIS TERRA launch: 1999-12-18
##            MODIS AQUA launch: 2002-05-04

modis.date <- as.Date("2002-07-03")
data <- subset(data, date > modis.date)


################################################################################
### Remove observations without coordinates ####################################
################################################################################

data <- data[!is.na(data$coordN | data$coordW),]


################################################################################
### Calculate number of species ################################################
################################################################################

data$nr.of.species <- apply(data,
                              1,
                              function(x) sum(!is.na(x[9:ncol(data)])))

data <- cbind(data[1:8],
                data$nr.of.species,
                data[10:ncol(data)-1])

colnames(data)[9] <- "nr.of.species"

################################################################################
### Plot single species ########################################################
################################################################################

# plot(data[,9],data$date, type="p")
# 
# qplot(x=data[,9], 
#       y=date, 
#       data=data,
#       geom="jitter",
#       main=colnames(data[9]),
#       xlab="Prävalenz",
#       ylab="Zeit")

################################################################################
### Transform UTM to LatLong coordinates #######################################
################################################################################

data.sp <- data

coordinates(data.sp) <- c("coordW", "coordN")
projection(data.sp) <- "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
data.sp <- spTransform(data.sp, CRS("+proj=longlat"))

data.sp <- as.data.frame(data.sp)

names(data.sp)[1] <- "lon"
names(data.sp)[2] <- "lat"

## Recombine dataframes
data.all.sp <- cbind(data[1:3],
                   data[6:9],
                   data.sp$lon,
                   data.sp$lat,
                   data$coordW,
                   data$coordN,
                   data[10:ncol(data)])

names(data.all.sp)[8] <- "lon"
names(data.all.sp)[9] <- "lat"
names(data.all.sp)[10] <- "coordW"
names(data.all.sp)[11] <- "coordN"


################################################################################
### Write new csv ##############################################################
################################################################################

write.table(data.all.sp, file = file.data.out, 
            dec = ",", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")
