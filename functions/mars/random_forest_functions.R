# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  COM3018/COMM053
#
# Prof. Nick F Ryman-Tubb
# The Surrey Business School
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# 16 OCTOBER 2019
#
# UPDATE
# 1.00      15/2/2019    Initial Version
# 1.01      25/2/2019    Updates for MANM module
# 1.02      16/10/2019   COM3018 / COMM053 2019
# 1.03      22/10/2019   Added PerformanceAnalytics as a required library
# 1.04      12/10/2020   Updated for R 4.x
# ************************************************
# R Script For lab 3

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
# Global Environment variables
# - i.e. available to all functions
# Good practice to place "constants" in named variables
# I use UPPERCASE to identify these in my code



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
               "PerformanceAnalytics")



# User defined functions are next

FOREST_SIZE       <- 3000                 # Number of trees in the forest
MAX_NODES <-  NULL
MTRY <- 5



findOptimalTreeParameters <- function(dataset){
  forest_sizes <-  c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000, 3000, 4000)
  max_nodes <-  c(5: 15, NULL)
  mtry <- c(1:10)
  
  

  

  #Determine best mtry
  for(i in 1:length(mtry)){
    FOREST_SIZE <-  1000
    MAX_NODES <- NULL
    MTRY <-  mtry[i]
    
    results <-  kfold(dataset, 5, randomForest)
    results <-  c(results, FOREST_SIZE=FOREST_SIZE, MAX_NODES=MAX_NODES, MTRY=MTRY)

    if(i==1){
      allResults<-data.frame(MTRYTEST=unlist(results))
      
    }else{
      allResults<-cbind(allResults,data.frame(MTRYTEST=unlist(results)))
    }
  }
  allResults<-data.frame(t(allResults))
  print(formattable::formattable(allResults))
  
  
  
  # 
  # 
  # #Determine best mtry
  # for(i in 1:length(max_nodes)){
  #   FOREST_SIZE <-  1000
  #   MAX_NODES <- max_nodes[i]
  #   MTRY <-  5
  #   
  #   results <-  kfold(dataset, 5, randomForest)
  #   results <-  c(results, FOREST_SIZE=FOREST_SIZE, MAX_NODES=MAX_NODES, MTRY=MTRY)
  #   
  #   if(i==1){
  #     allResults<-data.frame(MTRYTEST=unlist(results))
  #     
  #   }else{
  #     allResults<-cbind(allResults,data.frame(MTRYTEST=unlist(results)))
  #   }
  # }
  # allResults<-data.frame(t(allResults))
  # print(formattable::formattable(allResults))
  # 
  # 
  # 
  # 
  # #Determine best mtry
  # for(i in 1:length(forest_sizes)){
  #   FOREST_SIZE <-  forest_sizes[i]
  #   MAX_NODES <- NULL
  #   MTRY <-  5
  #   
  #   results <-  kfold(dataset, 5, randomForest)
  #   results <-  c(results, FOREST_SIZE=FOREST_SIZE, MAX_NODES=MAX_NODES, MTRY=MTRY)
  #   
  #   if(i==1){
  #     allResults<-data.frame(MTRYTEST=unlist(results))
  #     
  #   }else{
  #     allResults<-cbind(allResults,data.frame(MTRYTEST=unlist(results)))
  #   }
  # }
  # allResults<-data.frame(t(allResults))
  # print(formattable::formattable(allResults))
  # 
  # 
  
  
  
  
}






getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(results=measures,title=title)
  
  return(measures)
} #endof getTreeClassifications()


# ************************************************
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test, plot=TRUE, ...){
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 maxnodes=MAX_NODES,
                                 mtry=MTRY)
  
  if(plot==TRUE){
    #Visualise some trees
    print("Example tree")
    tree1 <- randomForest::getTree(rf, k=1, labelVar=TRUE)
    #tree2 <- getTree(rf, 42) 
    #tree3 <- getTree(rf, 100) 
    print(formattable::formattable(tree1))
    #reprtree:::plot.getTree(rf)
    
    
  }
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
} #endof randomForest()



# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Keeps all objects as local to this function
# ************************************************
main<-function(){
  
  keeps <-  c("TotalCharges", "MonthlyCharges", "tenure", "Contract_Monthtomonth", "InternetService_Fiber", "InternetService_TechSupport", "Contract_Twoyear", "Churn")
  
  dataset <- mars_GetPreprocessedDataset(FALSE)
  
  dataset <-  keepFields(dataset, keeps)
  
  ##UNCOMMENT OUT TO RUN, TAKES A LONG TIME. 
  #optimals <-  findOptimalTreeParameters(dataset)
  
  
  results <-  kfold(dataset, 5, randomForest)
  
  
  print(results)
  
} #endof main()



# ************************************************
# This is where R starts execution

# clears the console area
cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#This [optionally] sets working directory
#setwd("")

#Load additional R script files provide for this lab
source("functions/mars/data_pre_processing_pipeline.R")
source("functions/mars/data_pre_processing_functions.R")
source("functions/nick/4labfunctions.R")
source("functions/nick/lab4DataPrepNew.R")
source("functions/mars/utility_functions.R")

set.seed(123)

print("WELCOME TO LAB 3: PRACTICAL BUSINESS ANALYTICS")

# ************************************************
main()

print("end")

