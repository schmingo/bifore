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
### Import ls8.greyval ################################################################

ls8.greyval.deriv <- read.csv2("csv/hai/hai_greyvalues_NA_derivate.csv",
                               dec = ".",
                               header = TRUE,
                               stringsAsFactors = FALSE)

ls8.greyval <- read.csv2("csv/hai/hai_greyvalues_NA.csv",
                         dec = ".", 
                         header = TRUE,
                         stringsAsFactors = FALSE)


################################################################################
### Subsetting #################################################################

sub.ls8.greyval <- melt(ls8.greyval[, c(1, 7:ncol(ls8.greyval))],
                 id.vars = "Plotname",
                 variable.name = "Band")

sub.ls8.greyval <- sub.ls8.greyval[order(sub.ls8.greyval$Plotname), ]

sub.ls8.greyval.deriv <- melt(ls8.greyval.deriv[, c(1, 7:ncol(ls8.greyval.deriv))],
                       id.vars = "Plotname",
                       variable.name = "Band")

sub.ls8.greyval.deriv <- sub.ls8.greyval.deriv[order(sub.ls8.greyval.deriv$Plotname), ]

sub.select.ls8.greyval <- subset(sub.ls8.greyval,
                          Plotname %in% c("HEG01"))

sub.select.ls8.greyval.deriv <- subset(sub.ls8.greyval.deriv, Plotname %in% c("HEG01"))


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
                data = sub.select.ls8.greyval)

f1 <- geom_line(aes(x = Band, 
                    y = value, 
                    group = Plotname, 
                    colour = "f'(x)"),
                data = sub.select.ls8.greyval.deriv)

zero <- geom_hline(yintercept=0, 
                   linetype = "dashed",
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
