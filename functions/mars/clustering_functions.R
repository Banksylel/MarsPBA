# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#   DATA PRE-PROCESSING PIPELINE
#
#   
#   DATE:     11 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Endacott    Initial Version
# *************************************************************

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************


KFOLDS           <- 5

# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "stringr",
               "PerformanceAnalytics")

# Write ML Models here
plotKValueTests <- function(dataset){
  
  positionOutput<-which(names(dataset)==OUTPUT_FIELD)
  predictors<-dataset[,-positionOutput]
  
  # ************************************************
  # cluster plot of the 4 clusters (uses ggplot2 library)
  
  p<-factoextra::fviz_cluster(modelKmeans, data = predictors,geom = "point")
  print(p)
  
  results<-list()
  r<-1
  for (k in 2:5){
    modelKmeans <- kmeans(predictors, k, nstart=25)
    results[[r]]<-factoextra::fviz_cluster(modelKmeans, data = predictors,geom = "point") + ggtitle(paste("k=",k))
    r<-r+1
  }
  gridExtra:: grid.arrange(grobs=results, nrow = 2)
  
  p<-factoextra::fviz_nbclust(predictors, kmeans, method = "wss",k.max = 10)
  print(p)
  
  p<-factoextra::fviz_nbclust(predictors, kmeans, method = "silhouette",k.max = 15)
  print(p)
  
  gap_stat <- cluster::clusGap(predictors, FUN = kmeans, nstart = 25, K.max = 15, B = 50)
  p<-factoextra::fviz_gap_stat(gap_stat)
  print(p)
  
  
}


calculateClusterChurnRatios <- function(kmeansModel){
  
  K <-  length(kmeansModel$size)
  print(K)
  
  
}



######## Main Function ########

# ************************************************
# main() :
# Main entry point
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# ************************************************
main<-function(){
  
  # Read data from file
  dataset <- mars_GetPreprocessedDataset(FALSE)
  
  
  plotKValueTests(dataset)
  

  positionOutput<-which(names(dataset)==OUTPUT_FIELD)
  predictors<-dataset[,-positionOutput]
  
  modelKmeans <- kmeans(x=predictors, centers=5, nstart=25)
  
  print(str(modelKmeans))
  
  
  print(calculateClusterChurnRatios(modelKmeans))
  

 
  # The dataset for ML information
  print("End")
  
} #endof main()

# ************************************************
# This is where R starts execution

# Automatically release memory
gc()

# Clear plots and other graphics in RStudio output
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clear all warning messages
assign("last.warning", NULL, envir = baseenv())

# Clears the RStudio console area
cat("\014")

# If library not already on your computer this will download and
# install the library. Each library is activated.
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


#Source functions
source("functions/nick/lab3dataPrep.R")    # From Prof Nick's lab
source("functions/nick/lab2functions.R")   # From Prof Nick's lab
source("functions/mars/data_pre_processing_functions.R")
source("functions/mars/data_pre_processing_pipeline.R")

# Reset the pseudo-random number generator to start at the same point
set.seed(123)

print("PBA TEAM MARS: DATA PRE-PROCESSING PIPELINE")

# ************************************************
# Call the main function
main()

print("end")
