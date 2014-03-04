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
lib <- c("randomForest", "foreach")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Set species to test
species <- "Pnorisa.squalus"


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

## Select species data
species.df <- data.frame(data.raw[,names(data.raw) %in% c(species)])
names(species.df) <- species

## Replace NA-values by 0
species.df[is.na(species.df)] <- 0
tmp.species <- species.df
# 
# tmp.species.list <- as.list(t(species.df))
# tmp.species.list <- formatC(tmp.species.list, 
#                             width = 2, 
#                             format = "d", 
#                             flag = "0")
# 
# tmp.species <- as.data.frame(paste0("PR", tmp.species.list))
# names(tmp.species) <- species

## Create multiple dataframes with single species as predictor datasets
df.spec.greyval <- cbind(df.greyval, tmp.species)
df.spec.diff <- cbind(df.diff, tmp.species)
df.spec.greyval.diff <- cbind(df.greyval, df.diff, tmp.species)
df.spec.greyval.sd <- cbind(df.greyval, df.sd, tmp.species)
df.spec.diff.sd <- cbind(df.diff, df.sd, tmp.species)


## Define Random Forest input data #############################################
df.input.rf <- df.spec.greyval.diff ## Insert input dataset here!


################################################################################
### Random sample ##############################################################
################################################################################

## Split dataset | 3/4 train.data, 1/4 test.data      
set.seed(10)

index <- sample(1:nrow(df.input.rf), nrow(df.input.rf)*.75)
length(index)

train.data <- df.input.rf[index, ]
test.data <- df.input.rf[-index, ]


################################################################################
### Random Forest function #####################################################
### Classification - single species ############################################
################################################################################

predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])

## Function ####################################################################
train.rf <- randomForest(x = predictor_modisVAL,
                         y = response_speciesCLASS,
                         importance = TRUE,
                         ntree = 1000,
                         mtry = 2,
                         nodesize = 2,
                         type="classification",
                         do.trace = 100)

## Define output image | open image port
# png(paste0("images/randomForest_classification_", species, ".png"), 
#     width = 1024 * 6, 
#     height = 748 * 6, 
#     units = "px", 
#     res = 600)

# plot(randomForest(x = predictor_modisVAL,
#                   y = response_speciesCLASS,
#                   importance = TRUE,
#                   ntree = 1000,
#                   mtry = 2,
#                   nodesize = 2,
#                   type="classification",
#                   do.trace = 100))

## Close image port
# graphics.off()

print(train.rf)


################################################################################
### Prediction #################################################################
################################################################################


predict.df <- data.frame(predict(train.rf, test.data))

predict.df.comparison <- cbind(test.data[,ncol(test.data)], 
                               predict.df)

names(predict.df.comparison) <- c("real.classes", "predicted.classes")

predict.df.comparison

## Calculation of identical classes
notidentical <- which(test.data[,ncol(test.data)] != predict.df)
print(paste0((1-length(notidentical)/nrow(test.data))*100, "% of predicted classes are identical to real classification."))
