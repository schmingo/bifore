################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-03-02                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0300_biodiversity_data.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Subsetting data ############################################################
################################################################################

### Eliminate columns containing NA values and combine data in several ways ####
### to get different dataframe combinations ####################################

df.greyval.all <- data.raw[69:106]
df.diff.all <- data.raw[107:144]
df.sd.all <- data.raw[145:182]


## Create NA tables
df.na.greyval <- data.frame(colSums(is.na(df.greyval.all)))
names(df.na.greyval) <- c("NAs out of 225")

df.na.diff <- data.frame(colSums(is.na(df.diff.all)))
names(df.na.diff) <- c("NAs out of 225")

df.na.sd <- data.frame(colSums(is.na(df.sd.all)))
names(df.na.sd) <- c("NAs out of 225")


## Eliminate columns containing NA values
df.greyval <- cbind(data.raw[69:78], data.raw[87:106])  ## greyvalues
df.diff <- cbind(data.raw[108:116], data.raw[126:144])  ## diff
df.sd <- cbind(data.raw[145:154], data.raw[163:182])    ## sd 


################################################################################
### Combining data for randomForest ############################################
################################################################################

## Extract response column
tmp.speciesnr <- data.raw[9]

## Set columnname
names(tmp.speciesnr) <- "SpeciesNr"

## Create multiple dataframes with species number as predictor datasets
df.spnr.greyval <- cbind(df.greyval, tmp.speciesnr)
df.spnr.diff <- cbind(df.diff, tmp.speciesnr)
df.spnr.greyval.diff <- cbind(df.greyval, df.diff, tmp.speciesnr)
df.spnr.greyval.sd <- cbind(df.greyval, df.sd, tmp.speciesnr)
df.spnr.diff.sd <- cbind(df.diff, df.sd, tmp.speciesnr)


## Define Random Forest input data #############################################
df.input.rf <- df.spnr.greyval.diff ## Insert input dataset here!


################################################################################
### Random sample ##############################################################
################################################################################

## Create tmp.index column
tmp.index <- seq(1,nrow(df.input.rf))

df.input.tmp <- cbind(df.input.rf, tmp.index)


## Split dataset | 3/4 train.data, 1/4 test.data      
df.input.split <- split(df.input.tmp, df.input.tmp$tmp.index)

train.data <- sample(df.input.split, 
                     size = round(length(df.input.split)*0.75), 
                     replace = FALSE)

test.data <- df.input.tmp[!as.character(df.input.tmp$tmp.index) %in% names(train.data), ]

train.data <- as.data.frame(do.call("rbind", train.data), 
                            stringsAsFactors = FALSE)

## Remove tmp.index column
train.data <- train.data[,1:ncol(train.data)-1]
test.data <- test.data[,1:ncol(test.data)-1]

# names(train.data)
# names(test.data)


################################################################################
### Random Forest function #####################################################
### Regression - number of species #############################################
################################################################################

predictor.spnr <- train.data[,1:ncol(train.data)-1]
response.nofactor <- train.data[,ncol(train.data)]

## Function ####################################################################
train.rf <- randomForest(x = predictor.spnr,
                         y = response.nofactor,
                         importance = TRUE,
                         ntree = 500,
                         mtry = 5,
                         nodesize = 2,
                         type="regression",
                         do.trace = 100)
print(train.rf)

## Define output image | open image port
# png("images/randomForest_regression_speciesno.png", 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

plot(randomForest(x = predictor.spnr,
                  y = response.nofactor,
                  importance = TRUE,
                  ntree = 500,
                  mtry = 5,
                  nodesize = 2,
                  type="regression",
                  do.trace = 100))

## Close image port
# graphics.off()


print(train.rf)


################################################################################
### Prediction #################################################################
################################################################################

# ##  predict RInfo for new data set
# test2.predict <- predict(train.rf, test.data,index=2, na.rm=TRUE, progress="window", overwrite=TRUE, filename="ProbPred.png")
test.predict <- data.frame(predict(train.rf, test.data))
predict.compare <- cbind(test.data[,ncol(test.data)], 
                         test.predict,
                         abs(test.predict-test.data[,ncol(test.data)]))
names(predict.compare) <- c("real.values", "predicted.values", "diff.abs")

summary(predict.compare$diff)
