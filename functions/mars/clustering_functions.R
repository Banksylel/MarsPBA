# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   
#   DATE:     06 DECEMBER 2020
#   VERSION:  V1.04
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Endacott    Initial Version
#   1.01      15/11/2020    Brian Nyathi      + Generalisations
#   1.02      18/11/2020    Chris Endacott    Bug fixes
#   1.03      19/11/2020    Chris Endacott    More visualisations
#   1.04      06/12/2020    Chris Endacott    Changed colour defaults
# *************************************************************

# ************************************************
# Name      :   plotKValueTests() :
# Purpose   :   Plot the tests for determining the 'k' hyperparameter
#
# INPUT     :   data frame    - dataset     - the dataset to be clustered
#
# OUTPUT    :   none
#
# ************************************************
plotKValueTests <- function(dataset){
  
  #Remove the target field from the cluster dataset
  positionOutput<-which(names(dataset)==OUTPUT_FIELD)
  predictors<-dataset[,-positionOutput]

  #Create an empty list to store clustering results for each k value
  results<-list()
  
  #Visualise k-means plots for k=2:5 after applying PCA to reduce dimensions
  r<-1
  for (k in 2:5){
    modelKmeans <- kmeans(predictors, k, nstart=25)
    results[[r]]<-factoextra::fviz_cluster(modelKmeans, data = predictors,geom = "point") + ggtitle(paste("k=",k))
    r<-r+1
  }
  #Plot the visualisations
  gridExtra:: grid.arrange(grobs=results, nrow = 2)
  
  #Plot the elbow method chart for determining k
  p<-factoextra::fviz_nbclust(predictors, kmeans, method = "wss",k.max = 10)
  print(p)
  
  #Plot the silhouette method chart for determining k
  p<-factoextra::fviz_nbclust(predictors, kmeans, method = "silhouette",k.max = 15)
  print(p)
  
  #Plot the gap statistic chart for determining k
  gap_stat <- cluster::clusGap(predictors, FUN = kmeans, nstart = 25, K.max = 15, B = 50)
  p<-factoextra::fviz_gap_stat(gap_stat)
  print(p)
}


# ************************************************
# Name      :   visualiseClusters() :
# Purpose   :   Perform visualisation plotting on the clusters in a dataset defined by a trained k means model
#
# INPUT     :   data frame      - dataset                  - the clustered dataset
#           :   data frame      - kmeansModel              - the trained k means model

#
# OUTPUT    :   none
#
# ************************************************
visualiseClusters <- function(dataset, kmeansModel){
  
  #detmine the K value used in the model
  K <-  length(kmeansModel$size)

  #data frame for storing cluster field aggregates
  results <-  data.frame()
  
  #Iterate over each cluster to visualise and aggregate field values, storing results in 'results'
  for(i in 1:K){
    #Create a subset of the dataset with only the records in this cluster
    cluster<-dataset[which(kmeansModel$cluster==i),] 
    
    #Calculate and print churn statistics for the cluster
    numChurned <- nrow(cluster[which(cluster$Churn==1),])
    numRetained <-  nrow(cluster[which(cluster$Churn==0),])
    churnRatio <-  numChurned/(numRetained+numChurned)
    
    print(paste("Cluster ",i,": Churned: ", numChurned ,delim=""))
    print(paste("Cluster ",i,": Retained: ", numRetained ,delim=""))
    print(paste("Cluster ",i,": Churn ratio: ", churnRatio ,delim=""))
    
    #Calculate and print subscription rates for the cluster
    numMonthly <- nrow(cluster[which(cluster$Contract_Monthtomonth==1),])
    numYearly <- nrow(cluster[which(cluster$Contract_Oneyear==1),])
    numTwoYearly <- nrow(cluster[which(cluster$Contract_Twoyear==1),])
    
    print(paste("Contract types: Monthly:", numMonthly, ", One year", numYearly, "Two year", numTwoYearly ,delim=""))
    total <-  nrow(cluster)
    print(paste("Contract type percentages: Monthly:", numMonthly/total, ", One year", numYearly/total, "Two year", numTwoYearly/total ,delim=""))
    
    #Aggregate each cluster field and append it to results
    means <- round(colMeans(cluster),digits = 2)
    results <- rbind(results,cluster1=t(means))
    
    #Plot a tenure histogram for the cluster
    p<-ggplot(cluster, aes(x=tenure)) + geom_histogram(color = "black", binwidth = 1, fill="white", alpha=0.5)+
      xlim(0, 70)+
      ylim(0, 200)+
      ggtitle(paste("Tenure histogram for cluster",i))
    print(p)
    
    #Plot a monthlycharges histogram for the cluster
    p<-ggplot(cluster, aes(x=MonthlyCharges)) + geom_histogram(color = "pink", binwidth=3, fill="white", alpha=0.5)+
      xlim(0, 130)+
      ylim(0, 150)+
      ggtitle(paste("Monthly charges histogram for cluster",i))
    print(p)
    
    #Plot a total charges histogram for the cluster
    p<-ggplot(cluster, aes(x=TotalCharges)) + geom_histogram(color = "blue", binwidth = 200, fill="white", alpha=0.5)+
      xlim(0, 7000)+
      ylim(0, 200)+
      ggtitle(paste("Total charges histogram for cluster",i))
    print(p)
    
    
    
  }
  #Plot a graph of monthly charges against tenure coloured by cluster
  p<-ggplot(dataset, aes(x=tenure, y = MonthlyCharges)) + geom_point(color = COLOUR_PALLETE[kmeansModel$cluster])
  print(p)
  
  #Plot a graph of total charges against tenure coloured by cluster
  p<-ggplot(dataset, aes(x=tenure, y = TotalCharges)) + geom_point(color = COLOUR_PALLETE[kmeansModel$cluster])+ theme(legend.position="right")
  print(p) 

  #Plot graphs for cluster analysis
  plotClusterServiceSubscriptionRates(results)
  plotClusterChurnRates(results)
  plotClusterContractRates(results)
  plotClusterFamilyStatistics(results)
}


# ************************************************
# Name      :   plotClusterFamilyStatistics() :
# Purpose   :   Plot the family related statistics in a particular cluster
#
# INPUT     :   data frame      - results                  - data frame containing each cluster and its aggregated fields
#
# OUTPUT    :   none
#
# ************************************************
plotClusterFamilyStatistics <- function(results){
  
  #Get the aggregates we want to plot by cluster
  partner <- as.numeric(as.vector(results[,"Partner"]))
  dependents <- as.numeric(as.vector(results[,"Dependents"]))
  

  #Create the data frame of aggregates by cluster to plot
  df = melt(data.frame(HasPartner=partner, HasDependents=dependents,
                       experiment=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="Family")
  
  #Plot the graph
  p <- ggplot(df, aes(experiment, value, fill=Family)) + 
    geom_bar(position="dodge", stat='identity')+labs(
                                                     x ="Cluster", y = "Ratio")+ scale_fill_manual(values = COLOUR_PALLETE)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title = element_blank())+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  print(p)
  
}


# ************************************************
# Name      :   plotClusterChurnRates() :
# Purpose   :   Plot the cluster churn rates for each cluster
#
# INPUT     :   data frame      - results                  - data frame containing each cluster and its aggregated fields
#
# OUTPUT    :   none
#
# ************************************************
plotClusterChurnRates <- function(results){
  #Get the churn aggregates we want to plot by cluster
  yes <- as.numeric(as.vector(results[,"Churn"]))

  #Create the data frame of aggregates by cluster to plot
  df = melt(data.frame(Yes=yes,
                       experiment=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="Churn")
  
  #Plot the graph
  p <- ggplot(df, aes(experiment, value, fill='Churn', width=0.8, label=paste(toString(value*100),'%'))) + 
    geom_bar(position="dodge", stat='identity')+labs(
                                                     x ="Cluster", y = "Churn Rate")+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title = element_blank(),legend.position = "none" )+ scale_fill_manual(values = COLOUR_PALLETE)+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  print(p)
  
}




# ************************************************
# Name      :   plotClusterContractRates() :
# Purpose   :   Plot the contract rates in each cluster
#
# INPUT     :   data frame      - results                  - data frame containing each cluster and its aggregated fields
#
# OUTPUT    :   none
#
# ************************************************
plotClusterContractRates <- function(results){
  #Get the aggregates we want to plot by cluster
  oneMonth <- as.numeric(as.vector(results[,"Contract_Monthtomonth"]))
  oneYear <- as.numeric(as.vector(results[,"Contract_Oneyear"]))
  twoYear <- as.numeric(as.vector(results[,"Contract_Twoyear"]))

  #Create the data frame of aggregates by cluster to plot
  df = melt(data.frame(OneMonth=oneMonth, OneYear=oneYear,TwoYear=twoYear,
                       experiment=c("Cluster 1","Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="ContractLength")
 
  #Plot the graph
  p <- ggplot(df, aes(experiment, value, fill=ContractLength)) + 
    geom_bar(position="dodge", stat='identity')+labs(
                                                     x ="Cluster", y = "Contract length rate")+ scale_fill_manual(values = COLOUR_PALLETE)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title = element_blank())+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  print(p)
  
}


# ************************************************
# Name      :   plotClusterSubscriptionRates() :
# Purpose   :   Plot the service subscription rates in each cluster
#
# INPUT     :   data frame      - results                  - data frame containing each cluster and its aggregated fields
#
# OUTPUT    :   none
#
# ************************************************
plotClusterServiceSubscriptionRates <- function(results){
  
  addonsNames <- c("InternetService_OnlineSecurity", "InternetService_OnlineBackup", "InternetService_DeviceProtection","InternetService_TechSupport", "InternetService_StreamingTV", "InternetService_StreamingMovies")
  addonsDF <- results[,addonsNames]
  
  #Get the aggregates we want to plot by cluster
  onlineSecurity <- as.numeric(as.vector(results[,"InternetService_OnlineSecurity"]))
  onlineBackup <- as.numeric(as.vector(results[,"InternetService_OnlineBackup"]))
  deviceProtection <- as.numeric(as.vector(results[,"InternetService_DeviceProtection"]))
  techSupport <- as.numeric(as.vector(results[,"InternetService_TechSupport"]))
  streamingTV <- as.numeric(as.vector(results[,"InternetService_StreamingTV"]))
  streamingMovies <- as.numeric(as.vector(results[,"InternetService_StreamingMovies"]))
  
  #Create the data frame of aggregates by cluster to plot
  df = melt(data.frame(OnlineSecurity=onlineSecurity,OnlineBackup=onlineBackup, DeviceProtection = deviceProtection, TechSupport=techSupport, StreamingTV=streamingTV, StreamingMovies=streamingMovies,
                       experiment=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5")),
            variable_name="cluster")
  
  #Plot the graph
  p <- ggplot(df, aes(experiment, value, fill=cluster)) + 
    geom_bar(position="dodge", stat='identity')+labs(
                                                     x ="Service", y = "Subscription Rate")+ scale_fill_manual(values = COLOUR_PALLETE)+ theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title = element_blank() )+ scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  print(p)
}

# ************************************************
# Name      :   createKmeansModel() :
# Purpose   :   Create and train a k-Means model on a dataset
#
# INPUT     :   data frame      - dataset        - the dataset to cluster
#
# OUTPUT    :   object          - modelKmeans    - the k-Means model
#
# ************************************************
createKmeansModel<-function(dataset){

  #Remove the target field from the dataset to cluster
  positionOutput<-which(names(dataset)==OUTPUT_FIELD)
  predictors<-dataset[,-positionOutput]
  
  #Run the k-means model
  modelKmeans <- kmeans(x=predictors, centers=5, nstart=25)

  
  return(modelKmeans)
} 



# ************************************************
# Name      :   visualiseKmeansModel() :
# Purpose   :   visualise k means clustering done by a model
#
# INPUT     :   object          - kmeansModel            - the k-Means model
#           :   data frame      - unscaledDataset        - the unscaled data with raw values
#
# OUTPUT    :   none
#
# ************************************************
visualiseKmeansModel <- function(kmeansModel, unscaledDataset){
  #Retrieve the original dataset to get the original fields
  originalDataset <- NreadDataset(DATASET_FILENAME)
  rows <- as.integer(row.names(unscaledDataset))
  originalValueDataset <- unscaledDataset
  originalValueDataset[,c('tenure', 'MonthlyCharges', 'TotalCharges')] <- originalDataset[rows,c('tenure', 'MonthlyCharges', 'TotalCharges')]
  
  #Visualise the resulting clusters
  visualiseClusters(originalValueDataset,kmeansModel)
}



