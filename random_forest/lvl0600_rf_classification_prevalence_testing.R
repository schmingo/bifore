################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
## FURTHER PREVALENCE CALCULATIONS FOR ALL SPECIES                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-18                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
cat("\014")
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("randomForest", "foreach", "doParallel")
lapply(lib, function(...) require(..., character.only = TRUE))

ncores <- detectCores()-1

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")

## Timekeeping
starttime <- Sys.time()


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("csv/kili/lvl0400_rf_strat_prevalence_10.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Combining data for randomForest ############################################
################################################################################
set.seed(50)

## List species
lst.species <- names(data.raw[13:67])
lst.species

## Split incoming dataset
df.greyval <- data.raw[68:97]  ## greyvalues
df.diff <- data.raw[98:125]  ## diff
df.sd <- data.raw[126:155]  ## sd 

## Calculate randomForest for all Species
# registerDoParallel(cl <- makeCluster(ncores))
# 
# species.conf.matrix <- foreach(s = lst.species, 
#                                .combine = "cbind", 
#                                .packages = lib) %dopar% {

s <- lst.species[47]

## Select species data
df.species <- data.frame(data.raw[,names(data.raw) %in% c(s)])
names(df.species) <- s

tmp.species <- df.species

# summary(tmp.species)

## Create dataframe with single species as predictor dataset
df.spec.greyval <- cbind(df.greyval, tmp.species)



## Define Random Forest input data ###########################################
train.data <- df.spec.greyval


##############################################################################
### Random Forest function ###################################################
### Classification - single species ##########################################
##############################################################################

predictor_modisVAL <- train.data[,1:ncol(train.data)-1]
response_speciesCLASS <- as.factor(train.data[,ncol(train.data)])

## Initialize confusion matrix variables
conf.1.1 = NULL
conf.1.2 = NULL
conf.2.1 = NULL
conf.2.2 = NULL

## Function ##################################################################

#   foreach(i = seq(1:100)) %do% {

train.rf <- randomForest(x = predictor_modisVAL,
                         y = response_speciesCLASS,
                         importance = TRUE,
                         ntree = 500,
                         mtry = 2,
                         nodesize = 2,
                         type="classification",
                         do.trace = FALSE)

confusion.matrix = train.rf$confusion    

conf.1.1 = train.rf$confusion[1,1]
conf.1.2 = train.rf$confusion[1,2]
conf.2.1 = train.rf$confusion[2,1]
conf.2.2 = train.rf$confusion[2,2]
#   }

## Get mean values
mean(conf.1.1)
mean(conf.1.2)
mean(conf.2.1)
mean(conf.2.2)

## Save mean confusion matrix values into a dataframe
conf.matrix <- as.data.frame(c(mean(conf.1.1), 
                               mean(conf.1.2), 
                               mean(conf.2.1), 
                               mean(conf.2.2)))

## Set colnames (species)
names(conf.matrix) <- s

conf.matrix

confusion.matrix = train.rf$confusion    
confusion.matrix

train.rf$confusion[1,3]
train.rf$confusion[2,3]