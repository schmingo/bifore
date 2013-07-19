## Environmental stuff

## Clear workspace
rm(list = ls(all = TRUE))

# Working directory
setwd("/home/schmingo/Diplomarbeit/") #Linux
setwd("D:/Diplomarbeit/") #Windows

# Libraries
lib <- c("rgdal", "raster", "parallel")
lapply(lib, function(...) require(..., character.only = TRUE))


## Landsat data

# List files
fls.ls <- list.files("src/satellite/Landsat8_2013-07-07_hai/Level1_GeoTIFF_Data_Product/", 
                     pattern = ".TIF$", full.names = TRUE)

# # Reorder files
# tmp <- sapply(strsplit(substr(basename(fls.ls), 1, nchar(basename(fls.ls)) - 4), "_"), "[[", 2)
# fls.ls <- fls.ls[order(as.numeric(substr(tmp, 2, nchar(tmp))))]

# Import files as RasterLayer objects
rst.ls <- lapply(fls.ls, raster)
prj.ls <- CRS(projection(rst.ls[[1]]))


## Station data

# List center files
fls.hai.ctr <- list.files("src/csv/", pattern = "hai_plot_center.csv$", full.names = TRUE)

# Import center files as SpatialPointsDataframe objects
tbl.hai.ctr <- read.csv2(fls.hai.ctr, dec = ".", stringsAsFactors = FALSE)
coordinates(tbl.hai.ctr) <- c("Longitude", "Latitude")
projection(tbl.hai.ctr) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

tbl.hai.ctr <- spTransform(tbl.hai.ctr, CRS = prj.ls)

# List corner files
fls.hai <- list.files("src/csv/", pattern = "hai_corner.csv$", full.names = TRUE)

# Import corner files as SpatialPointsDataframe objects
tbl.hai <- read.csv2(fls.hai, dec = ".", stringsAsFactors = FALSE)
coordinates(tbl.hai) <- c("Longitude", "Latitude")
projection(tbl.hai) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
tbl.hai <- spTransform(tbl.hai, CRS = prj.ls)

# Retrieve extent from corner coordinates
ext.hai <- lapply(seq(1, nrow(tbl.hai), 4), function(i) {
  extent(coordinates(tbl.hai[i:(i+3), ]))
})

# Parallelization
clstr <- makePSOCKcluster(n.cores <- 4)
clusterExport(clstr, c("lib", "rst.ls", "ext.hai", "tbl.hai.ctr", "tbl.hai"))
clusterEvalQ(clstr, lapply(lib, function(i) require(i, character.only = TRUE, quietly = TRUE)))

# Extract and average cell values
val.hai <- parLapply(clstr, rst.ls, function(h) {
  tmp.val <- sapply(ext.hai, function(i) {
    tmp.xtr <- extract(h, i)
    
    if (length(tmp.xtr) > 1)
      tmp.xtr <- mean(tmp.xtr, na.rm = TRUE)
    
    return(tmp.xtr)
  })
  
  tmp.df <- data.frame(tbl.hai.ctr, ls_grey_value = tmp.val)
  
  return(tmp.df)
})

# Merge single data frames
val.hai.all <- Reduce(function(...) merge(..., by = 1:6), val.hai)
names(val.hai.all)[7:18] <- sapply(strsplit(substr(basename(fls.ls), 1, nchar(basename(fls.ls)) - 4), "_"), "[[", 2)
coordinates(val.hai.all) <- c("Longitude", "Latitude")

# Deregister parallel backend
stopCluster(clstr)

# show data
show(val.hai.all)
show(val.hai.all$B1)