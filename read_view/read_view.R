## READ AND VIEW COORDINATES

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries


## set working directory
setwd("/home/schmingo/Diplomarbeit/") #Linux
setwd("D:/Diplomarbeit/") #Windows

## set filepaths
file.coords.alb <- "src/csv/alb_corner.csv"
file.coords.hai <- "src/csv/hai_corner.csv"
file.coords.sch <- "src/csv/sch_corner.csv"

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