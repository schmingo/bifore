## READ AND VIEW COORDINATES

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries


## set filepaths
file.coords.alb <- "D:/Dropbox/Diplomarbeit/Daten/src/coordinates/alb_corner.csv"
file.coords.hai <- "D:/Dropbox/Diplomarbeit/Daten/src/coordinates/hai_corner.csv"
file.coords.sch <- "D:/Dropbox/Diplomarbeit/Daten/src/coordinates/sch_corner.csv"

## set working directory
#setwd(D:/Dropbox/Diplomarbeit/Daten&read_view/)

## read data
coords.alb <- read.csv(file.coords.alb, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)
coords.hai <- read.csv(file.coords.hai, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)
coords.sch <- read.csv(file.coords.sch, header = TRUE, sep = ";",dec = ".",
                       fill = FALSE, stringsAsFactors = FALSE)

## filter data


## show data
str(coords.alb)
str(coords.hai)
str(coords.sch)

summary(coords.alb)
summary(coords.hai)
summary(coords.sch)