################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Import and view csv files                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-11-28                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "raster", "sp")
lapply(lib, function(...) require(..., character.only = TRUE))


## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/")

## set filepaths
file.abundance.csv <- "src/csv/kili/abundance_matrix_hemp.csv"
file.data.out <- "src/csv/kili/abundance_data_subset.csv"


################################################################################
## read data
data <- read.csv2(file.abundance.csv, 
                  header = TRUE, 
                  sep = ";",
                  dec = ".",
                  fill = FALSE, 
                  stringsAsFactors = FALSE)


# data$date <- as.POSIXct(data$date, format="%m/%d/%Y")
data$date <- as.Date(data$date, format="%m/%d/%Y")


################################################################################
## replace 0-values with NA

# subset data
data.species <- data[,9:ncol(data)]

# set 0-values to NA
data.species[data.species==0] <- NA

# recombine data
data[,9:ncol(data)] <- data.species


################################################################################
## remove observations before MODIS satellite launch
##      Note: MODIS TERRA launch: 1999-12-18
##            MODIS AQUA launch: 2002-05-04

modis.date <- as.Date("2002-01-01")
data <- subset(data, date > modis.date)


################################################################################
## remove observations without coordinates

data <- data[!is.na(data$coordN | data$coordW),]


################################################################################
## remove species with less than 10 observations

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
## plot single species

plot(data10[,9],data10$date, type="p")

qplot(x=data10[,9], 
      y=date, 
      data=data10,
      geom="jitter",
      main=colnames(data10[9]),
      xlab="Prävalenz",
      ylab="Zeit")

################################################################################
## write new csv

write.table(data10, file = file.data.out, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")


################################################################################
## spatial stuff

# data10.sp <- data10
# 
# coordinates(data10.sp) <- ~coordW + coordN
# 
# spplot(data10.sp, zcol = "asl")
# 
# files <- list.files("D:/modiscloud", pattern = ".tif", recursive = TRUE,
#                     full.names = TRUE)
# 
# stck <- stack(files)
# 
# cloud.vals <- extract(stck, data10.sp) # matrix mit spalten = einzelne Bilder, Zeilen = die einzelnen Plots.