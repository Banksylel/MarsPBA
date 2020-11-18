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

plotKValueTests <- function(dataset){
  
  positionOutput<-which(names(dataset)==OUTPUT_FIELD)
  predictors<-dataset[,-positionOutput]

  
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


visualiseClusters <- function(dataset, kmeansModel){
  
  K <-  length(kmeansModel$size)
  print(K)
  
  results <-  data.frame()
  for(i in 1:K){
    cluster<-dataset[which(kmeansModel$cluster==i),] 
    
    numChurned <- nrow(cluster[which(cluster$Churn==1),])
    numRetained <-  nrow(cluster[which(cluster$Churn==0),])
    
    print(paste("Cluster ",i,": Churned: ", numChurned ,delim=""))
    print(paste("Cluster ",i,": Retained: ", numRetained ,delim=""))
    
    churnRatio <-  numChurned/(numRetained+numChurned)
    
    print(paste("Cluster ",i,": Churn ratio: ", churnRatio ,delim=""))
    
    numMonthly <- nrow(cluster[which(cluster$Contract_Monthtomonth==1),])
    numYearly <- nrow(cluster[which(cluster$Contract_Oneyear==1),])
    numTwoYearly <- nrow(cluster[which(cluster$Contract_Twoyear==1),])
    
    
    print(paste("Contract types: Monthly:", numMonthly, ", One year", numYearly, "Two year", numTwoYearly ,delim=""))
    total <-  nrow(cluster)
    print(paste("Contract type percentages: Monthly:", numMonthly/total, ", One year", numYearly/total, "Two year", numTwoYearly/total ,delim=""))
    
    
    
    info <- summary(cluster)
    means <-  unlist(info[4,])
    results <-rbind(results, data.frame(means))
    
    
    p<-ggplot(cluster, aes(x=tenure)) + geom_histogram(color = "black", binwidth = 1, fill="white", alpha=0.5)+
      xlim(0, 70)+
      ylim(0, 200)+
      ggtitle(paste("Tenure histogram for cluster",i))
    print(p)
    p<-ggplot(cluster, aes(x=MonthlyCharges)) + geom_histogram(color = "pink", binwidth=3, fill="white", alpha=0.5)+
      xlim(0, 130)+
      ylim(0, 150)+
      ggtitle(paste("Monthly charges histogram for cluster",i))
    print(p)
    p<-ggplot(cluster, aes(x=TotalCharges)) + geom_histogram(color = "blue", binwidth = 200, fill="white", alpha=0.5)+
      xlim(0, 7000)+
      ylim(0, 200)+
      ggtitle(paste("Total charges histogram for cluster",i))
    print(p)
    
    
    
  }
  print(results)
  p<-ggplot(dataset, aes(x=tenure, y = MonthlyCharges)) + geom_point(color = factor(kmeansModel$cluster))
  print(p)
  
  p<-ggplot(dataset, aes(x=tenure, y = TotalCharges)) + geom_point(color = factor(kmeansModel$cluster))
  print(p)
  
  print(p)
  
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
createKmeansModel<-function(dataset){

  #plotKValueTests(dataset)
  

  positionOutput<-which(names(dataset)==OUTPUT_FIELD)
  predictors<-dataset[,-positionOutput]
  
  modelKmeans <- kmeans(x=predictors, centers=5, nstart=25)
  
  print(str(modelKmeans))
  
  originalDataset <- mars_GetPreprocessedDataset(scaleflag = FALSE, printflag=FALSE)
  
  visualiseClusters(originalDataset,modelKmeans)
  

 
  # The dataset for ML information
  return(modelKmeans)
  
} #endof main()

# If library not already on your computer this will download and
# install the library. Each library is activated.
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


#Source functions
source("functions/nick/lab3dataPrep.R")    # From Prof Nick's lab
source("functions/nick/lab2functions.R")   # From Prof Nick's lab
source("functions/mars/data_pre_processing_functions.R")
source("functions/mars/data_pre_processing_pipeline.R")


