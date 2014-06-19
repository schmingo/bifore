cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##    
## Compare downloaded MYD02 with nocloud dates
## 
##  Version: 2014-02-18
##  
################################################################################
##
##  Copyright (C) 2014 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

lib <- c()
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Code/bifore/src/")


### Set filepaths ##############################################################

path.biodiversity.csv <- "csv/kili/lvl0100_biodiversity_data.csv"

path.myd02 <- "/home/schmingo/Daten/Code/bifore_src/myd02-03_hdf/"
path.myd03 <- "/home/schmingo/Daten/Code/bifore_src/myd03-35_hdf_daytime/"
path.myd02_03 <- "/home/schmingo/Daten/Code/bifore_src/myd02-03_hdf/"


### Import biodiversity dataset ################################################

data <- read.csv2(path.biodiversity.csv,
                  dec = ",",
                  header = TRUE, 
                  stringsAsFactors = FALSE)


### Extract MYD02 date string ##################################################

## List hdf files
lst.1km <- list.files(path.myd02,
                      pattern = "MYD021KM",
                      full.names = TRUE)
  
lst.qkm <- list.files(path.myd02,
                      pattern = "MYD02QKM",
                      full.names = TRUE)

lst.hkm <- list.files(path.myd02,
                      pattern = "MYD02HKM",
                      full.names = TRUE)


## Extract date string from filename
dates.1km <- substr(basename(lst.1km), 11, 22)
dates.1km <- paste0(substr(dates.1km, 0, 4),
                    "-",
                    substr(dates.1km, 5, 7),
                    "_",
                    substr(dates.1km, 9, 13))

dates.qkm <- substr(basename(lst.qkm), 11, 22)
dates.qkm <- paste0(substr(dates.qkm, 0, 4),
                    "-",
                    substr(dates.qkm, 5, 7),
                    "_",
                    substr(dates.qkm, 9, 13))

dates.hkm <- substr(basename(lst.hkm), 11, 22)
dates.hkm <- paste0(substr(dates.hkm, 0, 4),
                    "-",
                    substr(dates.hkm, 5, 7),
                    "_",
                    substr(dates.hkm, 9, 13))


dates.1km
dates.qkm
dates.hkm


### Check if downloaded MYD02 files are complete ###############################

data$date_nocloud %in% dates.1km
data$date_nocloud %in% dates.qkm
data$date_nocloud %in% dates.hkm

dates.1km %in% data$date_nocloud
dates.qkm %in% data$date_nocloud
dates.hkm %in% data$date_nocloud


### Copy MYD03 files ###########################################################

lst.myd03 <- list.files(path.myd03, pattern="MYD03", full.names=TRUE)

# substr(basename(lst.myd03[1]), 8, 19)
# substr(basename(lst.1km[1]), 11, 22)


for (i in lst.myd03)
  for (h in lst.1km)
    if (substr(basename(i), 8, 19) == substr(basename(h), 11, 22))
      file.rename(from = i,
                  to = paste0(path.myd02_03, basename(i)))


### Copy MYD02 files ###########################################################

for (g in lst.1km)
  file.rename(from = g,
              to = paste0(path.myd02_03, basename(g)))

for (f in lst.hkm)
  file.rename(from = f,
              to = paste0(path.myd02_03, basename(f)))

for (e in lst.qkm)
  file.rename(from = e,
              to = paste0(path.myd02_03, basename(e)))

