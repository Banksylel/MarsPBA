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
#   1.10      15/11/2020    Brian Nyathi      + Generalisations
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
    
    
    means <- round(colMeans(cluster),digits = 2)
    results <- rbind(results,cluster1=t(means))
    
    
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
  print(formattable::formattable(results))
  p<-ggplot(dataset, aes(x=tenure, y = MonthlyCharges)) + geom_point(color = factor(kmeansModel$cluster))
  print(p)
  
  p<-ggplot(dataset, aes(x=tenure, y = TotalCharges)) + geom_point(color = factor(kmeansModel$cluster))+ theme(legend.position="right")
  
  print(p) 

  plotClusterServiceSubscriptionRates(results)
  plotClusterChurnRates(results)
  plotClusterContractRates(results)
  plotClusterFamilyStatistics(results)
}

plotClusterFamilyStatistics <- function(results){
  partner <- as.numeric(as.vector(results[,"Partner"]))
  dependents <- as.numeric(as.vector(results[,"Dependents"]))
  

  df = melt(data.frame(HasPartner=partner, HasDependents=dependents,
                       experiment=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="Family")
  
  p <- ggplot(df, aes(experiment, value, fill=Family)) + 
    geom_bar(position="dodge", stat='identity')+labs(title="Family statistics by ratio in clusters",
                                                     x ="Cluster", y = "Ratio")
  
  print(p)
  
}


plotClusterChurnRates <- function(results){
  yes <- as.numeric(as.vector(results[,"Churn"]))
  no <- as.numeric(as.vector(1-results[,"Churn"]))
  

  df = melt(data.frame(Yes=yes, No=no,
                       experiment=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="Churn")
  
  p <- ggplot(df, aes(experiment, value, fill=Churn)) + 
    geom_bar(position="dodge", stat='identity')+labs(title="Churn rates per cluster",
                                                     x ="Cluster", y = "Churn Rate")
  
  print(p)
  
}



plotClusterContractRates <- function(results){
  oneMonth <- as.numeric(as.vector(results[,"Contract_Monthtomonth"]))
  oneYear <- as.numeric(as.vector(results[,"Contract_Oneyear"]))
  twoYear <- as.numeric(as.vector(results[,"Contract_Twoyear"]))

  df = melt(data.frame(OneMonth=oneMonth, OneYear=oneYear,TwoYear=twoYear,
                       experiment=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="ContractLength")
  
  p <- ggplot(df, aes(experiment, value, fill=ContractLength)) + 
    geom_bar(position="dodge", stat='identity')+labs(title="Contract length rates per cluster",
                                                     x ="Cluster", y = "Contract length rate")
  
  print(p)
  
}
plotClusterServiceSubscriptionRates <- function(results){
  addonsNames <- c("InternetService_OnlineSecurity", "InternetService_OnlineBackup", "InternetService_DeviceProtection","InternetService_TechSupport", "InternetService_StreamingTV", "InternetService_StreamingMovies")
  addonsDF <- results[,addonsNames]
  
  #Add baseline for plot clarity
  addonsDF[,] <- addonsDF[,]+0.002
  
  cluster1 <- as.numeric(as.vector(addonsDF[1,]))
  cluster2 <- as.numeric(as.vector(addonsDF[2,]))
  cluster3 <- as.numeric(as.vector(addonsDF[3,]))
  cluster4 <- as.numeric(as.vector(addonsDF[4,]))
  cluster5 <- as.numeric(as.vector(addonsDF[5,]))
  
  df = melt(data.frame(Cluster1=cluster1, Cluster2=cluster2,Cluster3=cluster3, Cluster4=cluster4, Cluster5=cluster5,
                       experiment=c("Online Security","Online Backup","Device Protection","Tech Support","Streaming TV","Steaming Movies")),
            variable_name="cluster")
  
  p <- ggplot(df, aes(experiment, value, fill=cluster)) + 
    geom_bar(position="dodge", stat='identity')+labs(title="Service subscription rates per cluster",
                                                     x ="Service", y = "Subscription Rate")
  
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


