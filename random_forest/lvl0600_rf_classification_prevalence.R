################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## RANDOM FOREST FOR MODIS DATA                                               ##
## FURTHER PREVALENCE CALCULATIONS FOR ALL SPECIES                            ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-22                                                        ##
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
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/csv/kili/")

## Set filenames
file.out.confusion.species.10 <- "lvl0600_rf_prevalence_10_mean_confmatrix.csv"

## Timekeeping
starttime <- Sys.time()


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2("lvl0400_rf_strat_prevalence_10.csv",
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Combining data for randomForest ############################################
################################################################################
set.seed(50)

## List species
lst.species <- names(data.raw[13:55])
lst.species

## Split incoming dataset
df.greyval <- data.raw[56:85]  ## greyvalues
df.diff <- data.raw[86:113]  ## diff
df.sd <- data.raw[114:143]  ## sd 

## Calculate randomForest for all Species
registerDoParallel(cl <- makeCluster(ncores))

species.conf.matrix <- foreach(s = lst.species, .combine = "cbind", .packages = lib) %dopar% {
  
  #   species <- lst.species[47]
  
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
  conf.1.3 = NULL
  conf.2.1 = NULL
  conf.2.2 = NULL
  conf.2.3 = NULL
  
  ## Function ##################################################################
  
  foreach(i = seq(1:100)) %do% {
    
    train.rf <- randomForest(x = predictor_modisVAL,
                             y = response_speciesCLASS,
                             importance = TRUE,
                             ntree = 500,
                             #                              mtry = 5,
                             nodesize = 2,
                             type="classification",
                             do.trace = FALSE)
    
    conf.1.1[i] = train.rf$confusion[1,1]
    conf.1.2[i] = train.rf$confusion[1,2]
    conf.1.3[i] = train.rf$confusion[1,3]
    conf.2.1[i] = train.rf$confusion[2,1]
    conf.2.2[i] = train.rf$confusion[2,2]
    conf.2.3[i] = train.rf$confusion[2,3]
    
  }
  
  ## Get mean values
  mean(conf.1.1)
  mean(conf.1.2)
  mean(conf.1.3)
  mean(conf.2.1)
  mean(conf.2.2)
  mean(conf.2.3)
  
  ## Save mean confusion matrix values into a dataframe
  conf.matrix <- as.data.frame(c(mean(conf.1.1), 
                                 mean(conf.1.2),
                                 mean(conf.1.3),
                                 mean(conf.2.1), 
                                 mean(conf.2.2),
                                 mean(conf.2.3),
                                 colSums(tmp.species)))
  
  ## Set colnames (species)
  names(conf.matrix) <- s
  return(conf.matrix)
  
}


## Close parallel backend
stopCluster(cl)


## Set rownames for new dataframe
df.rownames <- as.data.frame(c("Actual=0, predict=0",
                               "Actual=0, predict=1",
                               "Class.error 0",
                               "Actual=1, predict=0",
                               "Actual=1, predict=1",
                               "Class.error 1",
                               "no.of.species"))
names(df.rownames) <- "confusion.matrix"

species.conf.matrix <- cbind(df.rownames,
                             species.conf.matrix)

## Timekeeping
endtime <- Sys.time()
time <- endtime - starttime
time

## write confusion matrix table
write.table(species.conf.matrix, 
            file = file.out.confusion.species.10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")
