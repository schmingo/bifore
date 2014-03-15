################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK AND MODISCLOUD-PACKAGE  ##
##                                                                            ##
## Ref.: - http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf            ##
##       - MYD35 .hdf metadata                                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-15                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("modiscloud", "devtools", "doParallel", "rgdal", "raster", "ggplot2")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/")


################################################################################
### Set filepaths ##############################################################
################################################################################

## To preprocess MYD35_L2 and MYD03 files; both must be in the same directory.

path.biodiversity.csv <- ("Dropbox/Diplomarbeit/code/bifore/src/csv/kili/lvl0050_biodiversity_data.csv")

path.nocloud.csv <- ("Dropbox/Diplomarbeit/code/bifore/src/csv/kili/lvl0100_biodiversity_data.csv")

path.hdf.in <- ("/media/schmingo/Daten/Diplomarbeit/myd03-35_hdf/")

path.hdf.sub <- ("/media/schmingo/Daten/Diplomarbeit/myd03-35_hdf_daytime/")

path.tif.cloudmask <- ("/media/schmingo/Daten/Diplomarbeit/myd_cloudmask_tif_daytime/")

mrtpath <- ("/home/schmingo/apps/MRTswath/bin/swath2grid")


################################################################################
### Import biodiversity dataset ################################################
################################################################################

data <- read.csv2(path.biodiversity.csv,
                  dec = ",",
                  header = TRUE, 
                  stringsAsFactors = TRUE)

## Read date column as a date
data$date <- strftime(as.POSIXct(data$date, format="%Y-%m-%d"), format = "%Y%j")

data.orig <- data

## Check actual time
starttime <- Sys.time()
################################################################################
### Separate day and night hdf files ###########################################
################################################################################

fls.myd03 <- list.files(path.hdf.in,
                        pattern="MYD03",
                        full.names=TRUE)

fls.myd35 <- list.files(path.hdf.in,
                        pattern="MYD35",
                        full.names=TRUE)

registerDoParallel(cl <- makeCluster(8))

## MYD03
foreach(a = fls.myd03, .packages = lib) %dopar% {
  
  if (as.numeric(substr(basename(a),16,19)) < 1700)
    file.rename(from = a,
                to = paste0(path.hdf.sub, basename(a)))
}


## MYD35
foreach(b = fls.myd35, .packages = lib) %dopar% {
  
  if (as.numeric(substr(basename(b),19,22)) < 1700)
    file.rename(from = b,
                to = paste0(path.hdf.sub, basename(b)))
}

stopCluster(cl)


################################################################################
### Preprocessing MYD35_L2 and MYD03 | Run MRTSwath tool "swath2grid" ##########
################################################################################

## List MYD-files
list.files(path = path.hdf.sub, pattern = "MYD")

## Get the matching data/geolocation file pairs
fls.matching <- check_for_matching_geolocation_files(moddir = path.hdf.sub,
                                                     modtxt = "MYD35_L2",
                                                     geoloctxt = "MYD03",
                                                     return_geoloc = FALSE,
                                                     return_product = FALSE)
fls.matching


## Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76


################################################################################
### For-loop .hdf to .tif ######################################################
################################################################################

for(i in 1:nrow(fls.matching)) {
  ## Write parameter file for each .hdf
  prmfn <- write_MRTSwath_param_file(prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm",
                                     tifsdir=path.tif.cloudmask,
                                     modfn=fls.matching$mod35_L2_fns[i],
                                     geoloc_fn=fls.matching$mod03_fns[i],
                                     ul_lon=ul_lon,
                                     ul_lat=ul_lat,
                                     lr_lon=lr_lon,
                                     lr_lat=lr_lat)
  
  print(scan(file=prmfn, what="character", sep="\n"))
  
  ## hdf to raster using parameter file and subset box
  run_swath2grid(mrtpath="swath2grid",
                 prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm",
                 tifsdir=path.tif.cloudmask,
                 modfn=fls.matching$mod35_L2_fns[i],
                 geoloc_fn=fls.matching$mod03_fns[i],
                 ul_lon=ul_lon,
                 ul_lat=ul_lat,
                 lr_lon=lr_lon,
                 lr_lat=lr_lat)
}


################################################################################
### Check observations from csv file for cloudiness using tiffs ################
################################################################################

## get .tif list from swath2grid output
tiffns <- list.files(path.tif.cloudmask, pattern=".tif", full.names=TRUE)
tiffns

## Parallelization
registerDoParallel(cl <- makeCluster(4))
                   
## Convert data to spatial object
coordinates(data) <- ~ lon + lat
# coordinates(data) <- c("lon","lat")
proj4string(data) <- CRS("+init=epsg:4326")
                   
## Loop through all available dates
myd02.lst <- foreach(g = 1:nrow(data), .packages = lib, 
                     .combine = function(...) as.data.frame(rbind(...), 
                                                            stringsAsFactors = FALSE)) %dopar% {
  
  ## Initialize while-loop
  current.date <- data$date[g]
  cloudy <- TRUE
  
  
  while (cloudy) {
    
    ## List available *_b0.tif files of current date
    fls.avl <- tiffns[grep(current.date, substr(basename(tiffns), 1, 21))]
    fls.avl.b0 <- fls.avl[grep("b0", fls.avl)]
    
    ## Import raster images
    rst.avl.b0 <- lapply(fls.avl.b0, raster)
    
    for (h in rst.avl.b0) {
      
      ## Extract corresponding cell values from raster images
      val.avl.b0 <- extract(h, data[g, ])
      
      ## Convert extracted values to binary format
      bit.avl.b0 <- rev(t(digitsBase(val.avl.b0, base = 2, 8)))
      
      ## Cloud Indicator from myd35 metadata
      # Unobstructed FOV Quality Flag
      # 00 = Cloudy
      # 01 = Uncertain
      # 10 = Probably  Clear
      # 11 = Confident  Clear
      
      ## Break out of for-loop in case of cloud absence
      cloudindicator <- as.numeric(paste0(bit.avl.b0[2],bit.avl.b0[3]))
      
      if (cloudindicator == 11) {
        cloudy <- FALSE
        break
      }
    }
    
    ## Increment date by 1 day in case of no cloud absence
    if (cloudy) {
      
      current.date <- as.Date(current.date, format = "%Y%j") + 1
      current.date <- strftime(current.date, format = "%Y%j")
    }
  }
  
  ## Retrieve information about first cloud-free day
  return(substr(names(h), 11, 22))
}


## Deregister parallel backend
stopCluster(cl)

################################################################################
### Create new .csv with no-cloud date and diff-days ###########################
################################################################################
len <- length(names(data.orig))
len

data.clouddates <- data.orig

myd02.lst
names(myd02.lst) <- "date_nocloud"
names(data.clouddates)[2] <- "date_observation"

## Create no-cloud date
data.clouddates$date_nocloud <- myd02.lst$date_nocloud

# ## Create diff-days
data.clouddates$diff_days_nocloud <- as.numeric(as.Date(data.clouddates$date_nocloud, format = "%Y%j.%H%M") - as.Date(data.clouddates$date_observation, format = "%Y%j"))

## Reorder df
# data.orig <- data.orig[c(1,2,67,68,3:66)]
data.clouddates <- cbind(data.clouddates[1:2],
                         data.clouddates[ncol(data.clouddates)-1],
                         data.clouddates[ncol(data.clouddates)],
                         data.clouddates[3:11],
                         data.clouddates[12:len])
names(data.clouddates)


# ## Reformat dates
for (i in 1:nrow(data.clouddates)) {
  data.clouddates$date_nocloud[i] <- paste0(substr(data.clouddates$date_nocloud[i], 1, 4), 
                                      "-", 
                                      substr(data.clouddates$date_nocloud[i], 5, 7), 
                                      "_", 
                                      substr(data.clouddates$date_nocloud[i], 9, 13))
  
}

for (i in 1:nrow(data.clouddates)) {
  data.clouddates$date_observation[i] <- paste0(substr(data.clouddates$date_observation[i], 1, 4), 
                                          "-", 
                                          substr(data.clouddates$date_observation[i], 5, 7))                                         
}




## write new .csv
write.table(data.clouddates, 
            file = path.nocloud.csv,
            dec = ".",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

## plot diff_days_no-cloud
qplot(x=diff_days_nocloud,
      data=data.clouddates,
      geom="histogram",
      binwidth=0.5)



endtime <- Sys.time()

time <- endtime - starttime
time