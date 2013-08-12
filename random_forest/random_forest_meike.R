##################################################
#### random forest classification/regression #####
##################################################

# Clear workspace
rm(list=ls(all=TRUE))

## SET WORKING DIRECTORY
path.wd <- ("/home/mkuehnlein/randomForest/rf_rinfo/")
setwd(path.wd)

## anpassen !!!
path.in <- "/home/mkuehnlein/casestudies/rf_input/vp_03/day/om/"
path.out <- "/home/mkuehnlein/randomForest/rf_rinfo/results/vp03/day/"


## LOAD LIBRARY
library(latticeExtra)
library(randomForest)
# Libraries for parallelization
library(foreach)
library(doSNOW)
library(parallel)



# READ DATA
filename <- paste(paste(path.in,"rfInput_day_om_vp03_train_",sep=""),".dat",sep="")
readData <- read.table(filename, 
                       header=T, 
                       row.names=NULL, 
                       na.strings="-99.000000")

attach(readData) 
trainData <- data.frame(chDate,
                        x,
                        y,
                        B01_ca02,
                        B02_ca02,
                        B03_ca02,
                        #B01_ca01,
                        #B02_ca01,
                        #B03_ca01,
                        B04,
                        B05,
                        B06,
                        B07,
                        B08,
                        B09,
                        B10,
                        B11,
                        Tau,
                        Aef,
                        CWP,
                        B0103,
                        #B0409,
                        #B0406,
                        B0709,
                        B0910,
                        B0509,
                        B0610,
                        #SZen,
                        RInfo
                        #RProcess1,
                        #RProcess2,
                        #RProcess3,
                        #Rain
)
detach(readData)
names(trainData)

### RandomForest ###
set.seed(47)
## randomForest via 'parallel' package

# Number of cores
n.cores <- detectCores()

## Define desired parameters
n.tree <- 500
m.try <- 7

# Function parRandomForest
parRandomForest <- function(xx, ..., ntree=n.tree, mtry=m.try, importance=TRUE, do.trace=100, 
                            na.action=na.omit, ncores=n.cores, seed=47) {
  # Initialize Cluster
  cl <- makeCluster(ncores)
  # Initialize RNG and distribute streams to nodes
  if(!is.null(seed)) 
    clusterSetRNGStream(cl, seed)
  # Load randomForest package on cluster
  clusterEvalQ(cl, library(randomForest))
  
  # randomForest function for parLapply
  rfwrap <- function(xx, ntree, ...)
    randomForest(x=xx, ntree=ntree, ...)
  # Execute randomForest
  rfpar <- parLapply(cl, rep(ceiling(ntree/ncores), ncores), xx=xx, rfwrap, ...)
  
  # Stop cluster
  stopCluster(cl)
  
  # Combine resulting randomForest objects
  do.call(combine, rfpar)
}

# Call function
system.time(train.rf <- parRandomForest(trainData[,5:ncol(trainData)-1], 
                                        trainData[ , names(trainData) %in% c("RInfo")],
                                        ntree=n.tree, 
                                        mtry=m.try, 
                                        importance=TRUE, 
                                        na.action=na.omit))


## prediction ##
# READ DATA
filename <- paste(paste(path.in,"rfInput_day_om_vp03_test_",sep=""),".dat",sep="")
readData <- read.table(filename, 
                       header=T, 
                       row.names=NULL, 
                       na.strings="-99.000000")

attach(readData) 
testData <- data.frame(chDate,
                       x,
                       y,
                       B01_ca02,
                       B02_ca02,
                       B03_ca02,
                       #B01_ca01,
                       #B02_ca01,
                       #B03_ca01,
                       B04,
                       B05,
                       B06,
                       B07,
                       B08,
                       B09,
                       B10,
                       B11,
                       Tau,
                       Aef,
                       CWP,
                       B0103,
                       #B0409,
                       #B0406,
                       B0709,
                       B0910,
                       B0509,
                       B0610,
                       #SZen,
                       RInfo
                       #RProcess1,
                       #RProcess2,
                       #RProcess3,
                       #Rain
)
detach(readData)
names(testData)

##  predict RInfo for new data set
test.predict <- predict(train.rf, testData[,1:ncol(testData)])



## table prediction
predict.table <- table(actual=testData[ , names(testData) %in% c("RInfo")],
                       predicted=predict(train.rf,
                                         newdata=testData[,1:ncol(testData)-1],
                                         type="class"))
predict.table
capture.output(predict.table, file=paste(path.out.akt,"output_prediction.dat", sep = "/"),append = TRUE)


## matrix of class probabilities (one column for each class and one row for each input)
predictprop.table <- prop.table(predict.table, 1)
predictprop.table
capture.output(predictprop.table, file=paste(path.out.akt,"output_prediction.dat", sep = "/"),append = TRUE)


## writing prediction ##

## convert factor to array 
RInfo.predict <- as.character(test.predict)
result <- cbind(testData,RInfo.predict)
names(result)

# Add column - Rain -> 1, no rain -> 0 RInfo
result$RInfoInt <- ifelse(result[, NCOL(result) - 1] == "rain", 1, 0)

# Add column - Rain -> 1, no rain -> 0 RInfoPredicted
result$RInfoInt.predict <- ifelse(result[, NCOL(result) - 1] == "rain", 1, 0)
names(result)

# WRITE DATA
out.df <- data.frame(result$chDate,result$x,result$y,result$RInfo,result$RInfo.predict,result$RInfoInt,result$RInfoInt.predict)
write.table(out.df,file=paste(path.out.akt,"prediction.dat", sep = "/"),row.names = FALSE,col.names = FALSE,append = FALSE)


## copy Rout #####################
file.copy("rf_rinfoDay_om_prediction.Rout", path.out, overwrite = TRUE, copy.mode = TRUE)
file.remove("rf_rinfoDay_om_prediction.Rout")


