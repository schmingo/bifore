cat("\014")
################################################################################
##  
##  BiFoRe Scripts
##
##  Plot Variable Importance
##  - Mean Variable Importance
##  - Variable Importance Rank
##
##  
##  Version: 2015-03-16
##  
################################################################################
##
##  Copyright (C) 2015 Simon Schlauss (sschlauss@gmail.com)
##
##
##  This file is part of BiFoRe.
##  
##  BiFoRe is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##  
##  BiFoRe is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##  
##  You should have received a copy of the GNU General Public License
##  along with BiFoRe.  If not, see <http://www.gnu.org/licenses/>.
##  
################################################################################

## Clear workspace
rm(list = ls(all = TRUE))

## Required libraries
lib <- c("dplyr", "ggplot2")

lapply(lib, function(...) require(..., character.only = TRUE))


### Set filepaths ##############################################################

## Set working directory
setwd("D:/")

path.csv                  <- "Code/bifore/src/csv/lvl0400_2015-01-24/"
path.fig                  <- "Code/bifore/src/figures/"

file.in.importance        <- paste0(path.csv,"lvl0400_importance_25test.csv")

file.out.plot             <- paste0(path.fig,"lvl0400_variableImportance.png")
file.out.singlespec.mean  <- paste0(path.csv,"lvl0400_importance_mean_singlespec.csv")
file.out.allspec.mean     <- paste0(path.csv,"lvl0400_importance_mean_allspec.csv")
file.out.allspec.rank     <- paste0(path.csv,"lvl0400_importance_rank_allspec.csv")


### Import data ################################################################

data.raw <- read.csv2(file.in.importance,
                      dec = ",",
                      header = TRUE,
                      stringsAsFactors = FALSE)


### Multiplot function #########################################################

# Multiple plot function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


### Prepare data ###############################################################

## Remove varimp rank from dataset
df.varImp <- data.raw[-which(data.raw$parameters %in% data.raw$parameters[31:60]), ]


## Prepare Bandnames
for(i in seq(1:nrow(df.varImp))) {
  tmp.str <- unlist(strsplit(df.varImp$parameters[i],"_"))
  if (nchar(tmp.str[3]) < 2) {
    tmp.str[3] <- paste0("0",tmp.str[3])
  }
  df.varImp$parameters[i] <- paste("Band", 
                                   tmp.str[3], 
                                   sep = " ")
}

## Get mean variable importance (100 RF runs, 30 MODIS bands, 34 species)
df.varImp %>% 
  group_by(parameters) %>% 
  summarise_each(funs(mean), -c(1, 2)) %>%
  data.frame() -> df.varImp.mean


## Calculate mean variable importance (all species)
df.varImp.mean.all <- data.frame(cbind(df.varImp.mean[,1],
                                       rowMeans(df.varImp.mean[2:ncol(df.varImp.mean)])))
names(df.varImp.mean.all) <- c("Parameter", "Mean_VariableImportance")

## Transform dataframe (factor/numeric)
df.varImp.mean.all <- transform(df.varImp.mean.all, 
                                Mean_VariableImportance = as.numeric(Mean_VariableImportance),
                                Parameter = as.factor(Parameter))


## Create Ranking
tmp.df.varimp_rank <- data.frame(rank(-df.varImp.mean.all[,2], na.last = TRUE))
df.varImp.rank.all <- cbind(df.varImp.mean[,1],tmp.df.varimp_rank)
names(df.varImp.rank.all) <- c("Parameter", "VI_Rank")


### Plot #######################################################################

## Plot Mean Variable Importance
plot.a <- ggplot(df.varImp.mean.all, aes(x=Parameter, y=Mean_VariableImportance)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  scale_fill_grey() +
  xlab("") +
  ylab("Mean Variable Importance") +
  scale_y_continuous(breaks = seq(0,30,by = 5)) +
  theme_bw() +
  theme(axis.text.y = element_text(face = 'italic')) + 
  theme(panel.grid.major.x = element_line(colour = "black"))

plot.a

## Plot Variable Importance Rank
plot.b <- ggplot(df.varImp.rank.all, aes(x=Parameter, y=VI_Rank)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  scale_fill_grey() +
  xlab("") +
  ylab("Variable Importance Rank") +
  scale_y_continuous(breaks = seq(0,100,by = 5)) +
  theme_bw() +
  theme(axis.text.y = element_text(face = 'italic')) + 
  theme(panel.grid.major.x = element_line(colour = "black"))

plot.b

## Combine both plots 

## Write Plot
png(file.out.plot, 
    width = 1024 * 6, 
    height = 748 * 6, 
    units = "px", 
    res = 600)

multiplot(plot.a, plot.b, cols=2)

graphics.off()

### write csv ##################################################################

write.table(df.varImp.mean,
            file = file.out.singlespec.mean,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

write.table(df.varImp.mean.all,
            file = file.out.allspec.mean,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")

write.table(df.varImp.rank.all,
            file = file.out.allspec.rank,
            quote = FALSE,
            col.names = TRUE,
            row.names = FALSE,
            sep = ";",
            dec = ",")
