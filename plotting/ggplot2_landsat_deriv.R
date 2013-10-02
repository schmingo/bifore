################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PLOTTING EXTRACTED VALUES FROM LANDSAT8 SATELLITE DATA                     ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2013-09-30                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("ggplot2", "reshape2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import data ################################################################

data.deriv <- read.csv2("csv/hai/hai_greyvalues_landsat8_deriv.csv",
                        dec = ".",
                        header = TRUE, stringsAsFactors = FALSE)

data <- read.csv2("csv/hai/hai_greyvalues_landsat8.csv",
                  dec = ".", 
                  header = TRUE, stringsAsFactors = FALSE)


################################################################################
### Subsetting #################################################################

sub.data <- melt(data[, c(1, 7:ncol(data))], id.vars = "Plotname", variable.name = "Band")
sub.data <- sub.data[order(sub.data$Plotname), ]

sub.data.deriv <- melt(data.deriv[, c(1, 7:ncol(data.deriv))], id.vars = "Plotname", variable.name = "Band")
sub.data.deriv <- sub.data.deriv[order(sub.data.deriv$Plotname), ]

sub.select.data <- subset(sub.data, Plotname %in% c("HEG01"))
sub.select.data.deriv <- subset(sub.data.deriv, Plotname %in% c("HEG01"))


################################################################################
### Lineplot ###################################################################

## Define output image | open image port
png("images/landsat8_compare_DerivToRAW.png", 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

## Plot
f0 <- geom_line(aes(x = Band,
                    y = value,
                    group = Plotname,
                    colour = "f(x)"),
                data = sub.select.data)

f1 <- geom_line(aes(x = Band, 
                    y = value, 
                    group = Plotname, 
                    colour = "f'(x)"),
                data = sub.select.data.deriv)

zero <- geom_abline(linetype = "dashed",
                    colour = "#424242",
                    cex = 0.5)

lineplot <- ggplot() + f0 + f1 + zero +
            xlab("Landsat8 Bands") +
            ylab("greyvalue") +
            ggtitle("Vergleich Grauwert zu 1. Ableitung") +
            scale_colour_manual(values = c("red", "black"),
                                name = "HEG01 Landsat8",
                                breaks = c("f(x)", "f'(x)"))

lineplot

## Close image port
graphics.off()
