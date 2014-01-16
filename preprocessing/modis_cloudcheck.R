################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## CHECK COORDINATES FOR CLOUDS USING MODIS CLOUDMASK AND MODISCLOUD-PACKAGE  ##
##                                                                            ##
## Ref.: - http://modis-atmos.gsfc.nasa.gov/_docs/CMUSERSGUIDE.pdf            ##
##       - MOD35 .hdf metadata                                                ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-01-14                                                        ##
##                                                                            ##
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
# install.packages("MODIS", repos="http://R-Forge.R-project.org")
lib <- c("modiscloud", "devtools", "doParallel", "rgdal", "raster")
lapply(lib, function(...) require(..., character.only = TRUE))

## Set working directory
setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################

## To preprocess MOD35_L2 and MOD03 files; both must be in the same directory.

path.abundance.csv <- ("csv/kili/abundance_data_subset.csv")

path.cloudfree.csv <- ("csv/kili/cloudfree.csv")

path.hdf.in <- ("/home/schmingo/SAVE/Diplomarbeit/modiscloud_mod35_mod03/2002-2003/")

path.b0.cloudmask <- ("satellite/modiscloud_b0/")

mrtpath <- ("/home/schmingo/apps/MRTSwath/bin/swath2grid")


################################################################################
### Import dataset #############################################################

data <- read.csv2(path.abundance.csv,
                  dec = ".",
                  header = TRUE, 
                  stringsAsFactors = TRUE)

## Read date column as a date
#data$date <- as.Date(data$date, format="%Y-%m-%d")
#data$date <- as.POSIXct(data$date, format="%Y-%m-%d")
data$date <- strftime(as.POSIXct(data$date, format="%Y-%m-%d"), format = "%Y%j")

str(data[1:15])


################################################################################
### Preprocessing MOD35_L2 and MOD03 | Run MRTSwath tool "swath2grid" ##########

# # List MOD-files
# list.files(path = path.hdf.in, pattern = "MOD")
# 
# # Get the matching data/geolocation file pairs
# fns_df <- check_for_matching_geolocation_files(moddir = path.hdf.in,
#                                                modtxt = "MOD35_L2",
#                                                geoloctxt = "MOD03",
#                                                return_geoloc = FALSE,
#                                                return_product = FALSE)
# fns_df
# 
# 
# # Box to subset
# ul_lat <- -2.77
# ul_lon <- 36.93
# lr_lat <- -3.45
# lr_lon <- 37.76


################################################################################
### For-loop .hdf to .tif ######################################################
# 
# for(i in 1:nrow(fns_df)) {
#   # Write parameter file for each .hdf
#   prmfn <- write_MRTSwath_param_file(prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm",
#                                      tifsdir=path.tif.out,
#                                      modfn=fns_df$mod35_L2_fns[i],
#                                      geoloc_fn=fns_df$mod03_fns[i],
#                                      ul_lon=ul_lon,
#                                      ul_lat=ul_lat,
#                                      lr_lon=lr_lon,
#                                      lr_lat=lr_lat)
#   
#   print(scan(file=prmfn, what="character", sep="\n"))
#   
#   # hdf to raster using parameter file and subset box
#   run_swath2grid(mrtpath="swath2grid",
#                  prmfn="/home/schmingo/Diplomarbeit/tmpMRTparams.prm",
#                  tifsdir=path.tif.out,
#                  modfn=fns_df$mod35_L2_fns[i],
#                  geoloc_fn=fns_df$mod03_fns[i],
#                  ul_lon=ul_lon,
#                  ul_lat=ul_lat,
#                  lr_lon=lr_lon,
#                  lr_lat=lr_lat)
# }
# 

################################################################################
### Check observations from csv file for cloudiness using tiffs ################

## get .tif list from swath2grid output
tiffns <- list.files(path.b0.cloudmask, pattern=".tif", full.names=TRUE)
tiffns

## Parallelization
registerDoParallel(cl <- makeCluster(4))
                   
## Convert data to spatial object
coordinates(data) <- ~ lon + lat
# coordinates(data) <- c("lon","lat")
proj4string(data) <- CRS("+init=epsg:4326")
                   
## Loop through all available dates
# mod02.ddl <- foreach(g = 1:nrow(data), .packages = lib, .combine = "c") %dopar% {
mod02.ddl <- foreach(g = 1:nrow(data), .packages = lib, 
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
      
      ## Cloud Indicator from MOD35 metadata
      # Unobstructed FOV Quality Flag
      # 00 = Cloudy
      # 01 = Uncertain
      # 10 = Probably  Clear
      # 11 = Confident  Clear
      
      ## Break out of for-loop in case of cloud absence
      cloudindicator <- as.numeric(paste0(bit.avl.b0[2],bit.avl.b0[3]))
      if (cloudindicator == 10 | cloudindicator == 11) {
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
#   return(paste0(dirname(fls.avl)[1], "/", names(h), ".tif"))
#   return(current.date)
}


## Deregister parallel backend
stopCluster(cl)

## Write cloud-free dates and observation dates to .csv
mod02.ddl
names(mod02.ddl) <- "cloudfree_date"

## Add observation dates to table
mod02.ddl["obervation_date"] <- data$date
mod02.ddl.diff <- mod02.ddl

mod02.ddl.diff[,1] <- as.Date(mod02.ddl.diff[,1], format = "%Y%j.%H%M")
mod02.ddl.diff[,2] <- as.Date(mod02.ddl.diff[,2], format = "%Y%j")

mod02.ddl.diff["diff_date"] <- mod02.ddl.diff[,1] - mod02.ddl.diff[,2]

write.table(mod02.ddl.diff, 
            file = path.cloudfree.csv,
            dec = ".",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
################################################################################
### Download MODIS MOD02 files ### not supported by MODIS Package yet ! ########

# ?getHdf
# getProduct() 
# 
# MODISoptions(localArcPath = "/home/schmingo/Diplomarbeit/MODIS_ARC", 
#              outDirPath = "/home/schmingo/Diplomarbeit/MODIS_ARC/PROCESSED")
# 
# modis.products <- c("MOD021KM", "MOD02HKM", "MOD02QKM", "MOD03")
# 
# # Box to subset
# ul_lat <- -2.77
# ul_lon <- 36.93
# lr_lat <- -3.45
# lr_lon <- 37.76
# 
# 
# list.latlong <- list(xmax=lr_lon, xmin=ul_lon, ymin=lr_lat, ymax=ul_lat)
# 
# 
# mod02.ddl.end <- as.Date(mod02.ddl, format = "%Y%j") + 1
# mod02.ddl.end <- strftime(mod02.ddl.end, format = "%Y%j")
# 
# 
# 
# registerDoParallel(cl <- makeCluster(4))
# 
# foreach(g = mod02.ddl, h = mod02.ddl.end, .packages = lib) %dopar% {
# 
#   lapply(modis.products, function(i) {
# #     getHdf(product = i, begin = g, end = g, extent = list.latlong)
#     getHdf(product = "MOD021KM", begin = g, end = h, tileH = "21", tileV = "09")
#   })
# }
# 
# stopCluster(cl)
