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
## Version: 2014-02-18                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "raster", "sp", "rgdal")
lapply(lib, function(...) require(..., character.only = TRUE))


## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")

## Set filepaths
file.abundance.csv <- "src/csv/kili/abundance_matrix_hemp.csv"
file.data.out <- "src/csv/kili/lvl0050_biodiversity_data.csv"


################################################################################
### Read data ##################################################################
################################################################################

data <- read.csv2(file.abundance.csv, 
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
### Remove species with less than 10 observations ##############################
################################################################################

data.list <- split(data, data$plot)

test.list <- do.call("rbind", lapply(seq(data.list), function(i) {
  tmp.mat <- as.matrix(data.list[[i]][, 9:ncol(data.list[[i]])])
  t <- apply(tmp.mat, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

index.species10 <- which(apply(test.list, 2, sum, na.rm = TRUE) >= 10) + 8

data10 <- data[, c(1:8, index.species10)]


################################################################################
### Calculate number of species ################################################
################################################################################

data10$nr.of.species <- apply(data10,
                              1,
                              function(x) sum(!is.na(x[9:ncol(data10)])))

data10 <- cbind(data10[1:8],
                data10$nr.of.species,
                data10[10:ncol(data10)-1])

colnames(data10)[9] <- "nr.of.species"

################################################################################
### Plot single species ########################################################
################################################################################

# plot(data10[,9],data10$date, type="p")
# 
# qplot(x=data10[,9], 
#       y=date, 
#       data=data10,
#       geom="jitter",
#       main=colnames(data10[9]),
#       xlab="Prävalenz",
#       ylab="Zeit")

################################################################################
### Transform UTM to LatLong coordinates #######################################
################################################################################

data.sp <- data10

coordinates(data.sp) <- c("coordW", "coordN")
projection(data.sp) <- "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
data.sp <- spTransform(data.sp, CRS("+proj=longlat"))

data.sp <- as.data.frame(data.sp)

names(data.sp)[1] <- "lon"
names(data.sp)[2] <- "lat"

## Recombine dataframes
data10.sp <- cbind(data10[1:3],
                   data10[6:9],
                   data.sp$lon,
                   data.sp$lat,
                   data10$coordW,
                   data10$coordN,
                   data10[10:ncol(data10)])

names(data10.sp)[8] <- "lon"
names(data10.sp)[9] <- "lat"
names(data10.sp)[10] <- "coordW"
names(data10.sp)[11] <- "coordN"

################################################################################
### Write new csv ##############################################################
################################################################################

write.table(data10.sp, file = file.data.out, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")
