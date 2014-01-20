###############################################################################
## Download MODIS myd02 files ### not supported by MODIS Package yet ! ########

?getHdf
getProduct() 

MODISoptions(localArcPath = "/home/schmingo/Diplomarbeit/MODIS_ARC", 
             outDirPath = "/home/schmingo/Diplomarbeit/MODIS_ARC/PROCESSED")

modis.products <- c("MOD021KM", "MOD02HKM", "MOD02QKM", "MOD03")

# Box to subset
ul_lat <- -2.77
ul_lon <- 36.93
lr_lat <- -3.45
lr_lon <- 37.76


list.latlong <- list(xmax=lr_lon, xmin=ul_lon, ymin=lr_lat, ymax=ul_lat)


mod02.lst.end <- as.Date(mod02.lst, format = "%Y%j") + 1
mod02.lst.end <- strftime(mod02.lst.end, format = "%Y%j")



registerDoParallel(cl <- makeCluster(4))

foreach(g = mod02.lst, h = mod02.lst.end, .packages = lib) %dopar% {

  lapply(modis.products, function(i) {
#     getHdf(product = i, begin = g, end = g, extent = list.latlong)
    getHdf(product = "MOD021KM", begin = g, end = h, tileH = "21", tileV = "09")
  })
}

stopCluster(cl)