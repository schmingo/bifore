################################################################################
## BiFoRe Scripts                                                             ##
##                                                                            ##
## PREPARE DATASET FOR RANDOMFOREST - ALL OBSERVATIONS                        ##
##                                                                            ##
## Author: Simon Schlauss (sschlauss@gmail.com)                               ##
## Version: 2014-04-06                                                        ##
##                                                                            ##
################################################################################


## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("sampling")
lapply(lib, function(...) require(..., character.only = TRUE))

## set working directory
# setwd("/home/schmingo/Dropbox/Diplomarbeit/code/bifore/src/")
setwd("D:/Dropbox/Diplomarbeit/code/bifore/src/")


################################################################################
### Set filepaths ##############################################################
################################################################################

file.in.0300 <- "csv/kili/lvl0300_biodiversity_data.csv"
file.out.braunblanq.all <- "csv/kili/lvl0400_braun-blanquet_all.csv"
file.out.braunblanq.10 <- "csv/kili/lvl0400_braun-blanquet_10.csv"
file.out.specno.all <- "csv/kili/lvl0400_specno_all.csv"
file.out.specno.10 <- "csv/kili/lvl0400_specno_10.csv"
file.out.prevalence.all <- "csv/kili/lvl0400_prevalence_all.csv"
file.out.prevalence.10 <- "csv/kili/lvl0400_prevalence_10.csv"


################################################################################
### Import dataset #############################################################
################################################################################

data.raw <- read.csv2(file.in.0300,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


################################################################################
### Subsetting data ############################################################
################################################################################

### Eliminate columns containing NA values and combine data in several ways ####
### to get different dataframe combinations ####################################

df.sub.basics <- cbind(data.raw[1:8], data.raw[10:13])
df.sub.specno <- data.raw[9]
df.sub.species <- data.raw[14:178]
df.sub.greyval.all <- data.raw[179:216]
df.sub.diff.all <- data.raw[217:254]
df.sub.sd.all <- data.raw[255:292]


################################################################################
### Remove bands containing NA values ##########################################
################################################################################

## Create NA value tables
# df.na.greyval <- data.frame(colSums(is.na(df.sub.greyval.all)))
# names(df.na.greyval) <- c("NAs out of 225")
# 
# df.na.diff <- data.frame(colSums(is.na(df.sub.diff.all)))
# names(df.na.diff) <- c("NAs out of 225")
# 
# df.na.sd <- data.frame(colSums(is.na(df.sub.sd.all)))
# names(df.na.sd) <- c("NAs out of 225")


## Eliminate columns containing NA values
df.sub.greyval <- cbind(data.raw[179:188], data.raw[197:216])  ## greyvalues
df.sub.diff <- cbind(data.raw[218:226], data.raw[236:254])  ## diff
df.sub.sd <- cbind(data.raw[255:264], data.raw[273:292])    ## sd


## Recombine dataframes
data <- cbind(df.sub.basics,
              df.sub.specno, 
              df.sub.species,
              df.sub.greyval, 
              df.sub.diff, 
              df.sub.sd)

## Combine braun-blanquet table for single species modeling
data.rf.braunblanq.all <- cbind(df.sub.basics,
                                df.sub.species,
                                df.sub.greyval, 
                                df.sub.diff, 
                                df.sub.sd)

## write braun-blanquet table
write.table(data.rf.braunblanq.all, 
            file = file.out.braunblanq.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

################################################################################
### Remove species with less than 10 observations in different plots ###########
### Most abundant species ######################################################
################################################################################

data10.list <- split(data, data$plot)
data10.tmp.list <- do.call("rbind", lapply(seq(data10.list), function(i) {
  matrix <- as.matrix(data10.list[[i]][, 14:178])
  t <- apply(matrix, 2, sum, na.rm = TRUE)
  t[t == 0] <- NA
  t[t > 0] <- 1
  return(t)
}))

data10.species.index <- which(apply(data10.tmp.list, 2, sum, na.rm = TRUE) >= 10) + 13
data10 <- data[, c(1:13, data10.species.index, 179:ncol(data))]
names(data10)

## Subset data10 (most abundant species)
df.sub.10.basics <- data10[1:12]
df.sub.10.specno <- data10[13]
df.sub.10.species <- data10[14:68]
df.sub.10.greyval <- data10[69:98]
df.sub.10.diff <- data10[99:126]
df.sub.10.sd <- data10[127:156]


## Combine braun-blanquet table for single species modeling
## Most abundant species
data.rf.braunblanq.10 <- cbind(df.sub.10.basics,
                               df.sub.10.species,
                               df.sub.10.greyval,
                               df.sub.10.diff,
                               df.sub.10.sd)

## write braun-blanquet table for most abundant species
write.table(data.rf.braunblanq.10, 
            file = file.out.braunblanq.10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


# ## Test data10 (most abundant species)
# test.data10.matrix <- as.matrix(df.sub.10.species)
# test.data10.matrix[is.na(test.data10.matrix)] <- 0
# test.data10.matrix <- ifelse(test.data10.matrix >= 1,1,0)
# df.test.data10 <- as.data.frame(colSums(test.data10.matrix, na.rm = TRUE))
# 
# print(df.test.data10)
# summary(df.test.data10)


################################################################################
### Create number of species dataframe for RandomForest ########################
################################################################################

### For all species
data.rf.specno.all <- cbind(df.sub.basics,
                            df.sub.specno,
#                             df.sub.species,
                            df.sub.greyval,
                            df.sub.diff,
                            df.sub.sd)

## Write table - all species - bands containing NA values removed
write.table(data.rf.specno.all, 
            file = file.out.specno.all,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")

################################################################################
### For species with less than 10 observations in different plots
### Most abundant species

data.rf.specno.10 <- cbind(df.sub.10.basics,
                           df.sub.10.specno,
#                            df.sub.10.species,
                           df.sub.10.greyval,
                           df.sub.10.diff,
                           df.sub.10.sd)

## Write table - most abundant species - bands containing NA values removed
write.table(data.rf.specno.10, 
            file = file.out.specno.10,
            dec = ",",
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";")


# ################################################################################
# ### Create presence-absence df #################################################
# ################################################################################
# 
# ### For all species
# ## Read as matrix
# matrix.presabs.all <- as.matrix(df.sub.species)
# 
# ## Replace NA with 0
# matrix.presabs.all[is.na(matrix.presabs.all)] <- 0
# 
# ## Replace values >=1 with 1
# matrix.presabs.all <- ifelse(matrix.presabs.all >= 1,1,0)
# 
# ## Combine dataframes
# data.rf.presabs.all <- cbind(df.sub.basics,
#                          as.data.frame(matrix.presabs.all),
#                          df.sub.greyval,
#                          df.sub.diff,
#                          df.sub.sd)
# # names(data.rf.presabs.all)
# 
# write.table(data.rf.presabs.all,
#             file = file.out.presabs.all,
#             dec = ",",
#             quote = FALSE,
#             col.names = TRUE,
#             row.names = FALSE,
#             sep = ";")
# 
# ################################################################################
# ### For species with less than 10 observations in different plots
# 
# ## Read as matrix
# matrix.presabs.10 <- as.matrix(df.sub.10.species)
# 
# ## Replace NA with 0
# matrix.presabs.10[is.na(matrix.presabs.10)] <- 0
# 
# ## Replace values >=1 with 1
# matrix.presabs.10 <- ifelse(matrix.presabs.10 >= 1,1,0)
# 
# ## Combine dataframes
# data.rf.presabs.10 <- cbind(df.sub.10.basics,
#                          as.data.frame(matrix.presabs.10),
#                          df.sub.10.greyval,
#                          df.sub.10.diff,
#                          df.sub.10.sd)
# # names(data.rf.presabs.10)
# 
# write.table(data.rf.presabs.10,
#             file = file.out.presabs.10,
#             dec = ",",
#             quote = FALSE,
#             col.names = TRUE,
#             row.names = FALSE,
#             sep = ";")
# 
# 
# 
# 
# # ################################################################################
# # ### Calculate prevalence for all species #######################################
# # ################################################################################
# # 
# # ## Calculate prevalence for all species
# # prevalence.all <- data.frame(colSums(df.sub.species > 0, na.rm = TRUE), 
# #                              row.names = NULL)
# # 
# # ## Combine new df
# # df.prevalence.all.spec <- cbind(names(df.sub.species), prevalence.all)
# # 
# # ## Set colnames
# # colnames(df.prevalence.all.spec) <- c("species", "prevalence")
# # 
# # 
# # ## Write table
# # write.table(df.prevalence.all.spec, 
# #             file = file.out.prevalence.all,
# #             dec = ",",
# #             quote = FALSE,
# #             col.names = TRUE,
# #             row.names = FALSE,
# #             sep = ";")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ################################################################################
# # ### Remove species with less than 10 observations in different plots ###########
# # ################################################################################
# # 
# # data.list <- split(data, data$plot)
# # tst.list <- do.call("rbind", lapply(seq(data.list), function(i) {
# #   matrix <- as.matrix(data.list[[i]][, 14:178])
# #   t <- apply(matrix, 2, sum, na.rm = TRUE)
# #   t[t == 0] <- NA
# #   t[t > 0] <- 1
# #   return(t)
# # }))
# # 
# # index.species10 <- which(apply(tst.list, 2, sum, na.rm = TRUE) >= 10) + 13
# # data10 <- data[, c(1:13, index.species10, 178:ncol(data))]
# # names(data10)
# # 
# # ## Write table
# # write.table(data10, 
# #             file = file.out.specno.10,
# #             dec = ",",
# #             quote = FALSE,
# #             col.names = TRUE,
# #             row.names = FALSE,
# #             sep = ";")
# # 
# # 
# # 
# # 
# # 
# # ################################################################################
# # ### Stratified sampling - only one random observation per plot #################
# # ################################################################################
# # 
# # set.seed(50)
# # 
# # data.strat <- data[strata(data, 
# #                           stratanames = "plot", 
# #                           size = rep(1,length(unique(data$plot))),
# #                           method = "srswor")$ID_unit, ]
# # 
# # 
# # ## Write table
# # write.table(data.strat, 
# #             file = path.biodiversity.strat.plot,
# #             dec = ",",
# #             quote = FALSE,
# #             col.names = TRUE,
# #             row.names = FALSE,
# #             sep = ";")
# 
# 
# 
