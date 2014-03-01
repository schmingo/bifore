################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-02-25                                                        ##
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
### Number of species ##########################################################

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


################################################################################
### Random Forest function #####################################################
################################################################################

# MSE = Mean square error = Mittlere quadratische Abweichung
# ME
# MAE
# RMSE
# Rsq

# beschreibung vom datensatz
# min, max, median




################################################################################
### Regression - number of species #############################################

## Define Random Forest input data #############################################
df.input.rf.spnr <- df.spnr.greyval.diff ## Insert input dataset here!

predictor.spnr <- df.input.rf.spnr[,1:ncol(df.input.rf.spnr)-1]
response.factor <- as.factor(df.input.rf.spnr[,ncol(df.input.rf.spnr)])
response.nofactor <- df.input.rf.spnr[,ncol(df.input.rf.spnr)]

## Function ####################################################################
train.rf.spnr <- randomForest(x = predictor.spnr,
                              y = response.nofactor,
                              importance = TRUE,
                              ntree = 500,
                              mtry = 5,
                              nodesize = 2,
                              type="regression",
                              do.trace = 100)
print(train.rf.spnr)

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


print(train.rf.spnr)
################################################################################
### Prediction #################################################################
################################################################################

# ## Create test-df
# names(train.data[1:180,5:ncol(train.data)-1])
# testData <- train.data[181:225,3:ncol(train.data)-1]
# 
# 
# ##  predict RInfo for new data set
# test.predict <- predict(train.rf, testData,type="prob", index=2, na.rm=TRUE, progress="window", overwrite=TRUE, filename="ProbPred.png")
# test.predict <- data.frame(predict(train.rf, testData))
# test.predict

