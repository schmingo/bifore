################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## Import and view csv files                                                  ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-11-26                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2")
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

modis.date <- as.Date("2000-01-01")
data <- subset(data, date > modis.date)


################################################################################
## remove observations without coordinates

data <- data[!is.na(data$coordN | data$coordW),]


################################################################################
## remove species with less than 10 observations

x=0

# for loop to count not-na-values
for (i in data[,10]) {
  ifelse(i != "NA", 
         x <- x+1, 
         x == x)
  print(paste("plot",data$plot[i]," count:",x))
}


# for loop to print plot id's
for (i in data[,1]) {
  y <- data$plot[i]
  print(i)
}

# count na-vaules for one species
sum( !is.na(data[,9]))
################################################################################
## plot single species

plot(data[,10],data$date, type="p")

qplot(x=data[,10], 
      y=date, 
      data=data,
      geom="point",
      main=colnames(data[10]),
      xlab="Prävalenz",
      ylab="Zeit")

################################################################################
## write new csv

write.table(data, file = file.data.out, 
            dec = ".", 
            quote = FALSE, 
            col.names = TRUE, 
            row.names = FALSE, 
            sep =";")