# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   LOGISTIC REGRESSION FUNCTIONS
#
#   
#   DATE:     11 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.01      15/11/2020    Chris Jennings    Adopt k-folds
#   1.02      16/11/2020    Chris Jennings    Fixed bugs



#This code is hideous. 
#I promise I will clean it.


MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "stringr",
               "PerformanceAnalytics",
               "ggplot2",
               "reshape")


calculateAverageCustomerLifetime <- function(){
  rawDataset <- NreadDataset(DATASET_FILENAME)
  
  avgTenure <-  mean(rawDataset[,CUSTOMER_LIFETIME_FIELD])
  
  return(avgTenure)
  
}

estimateCustomerValue <- function(monthlyCharges, avgTenure){
  return(monthlyCharges*avgTenure)
}

evaluateModel <- function(predicted, expected, threshold, monthlyCharges, acquisitionCost,enticementPercent, minEnticementThreshold){
  avgTenure <-  calculateAverageCustomerLifetime()
  predictions<-ifelse(predicted<threshold,0,1)
  
  results<-list(  "TP"=0,
                  "FN"=0,
                  "TN"=0,
                  "FP"=0
  )  
  
  wronglyEnticedCost <- 0
  correctlyEnticedCost <- 0
  lostRevenueWithModel <- 0
  lostRevenueWithoutModel <- 0
  costToReplaceWithModel <- 0
  costToReplaceWithoutModel <- 0
  
  for(i in 1:length(predictions)){
    customerValue <-  estimateCustomerValue(monthlyCharges[i] ,avgTenure)
    
    if(predicted[i]==1){
      customerEnticementSpend <- customerValue*enticementPercent
      
      

      
      #False Positive, wrongly enticed customers
      if(expected[i]==0){
        results$FP <- results$FP + 1
        
        #if we spend
        if(monthlyCharges[i]>minEnticementThreshold){
          wronglyEnticedCost <- wronglyEnticedCost+customerEnticementSpend
          
        }
        

        
      #True Positive
      }else{
        results$TP <- results$TN + 1
        
        #if we spend
        if(monthlyCharges[i]>minEnticementThreshold){
          correctlyEnticedCost <- correctlyEnticedCost+customerEnticementSpend
          
        }else{
          costToReplaceWithModel <- costToReplaceWithModel + acquisitionCost
          lostRevenueWithModel <- lostRevenueWithModel + customerValue
        }
        
        
        costToReplaceWithoutModel <- costToReplaceWithoutModel + acquisitionCost
        lostRevenueWithoutModel <- lostRevenueWithoutModel+customerValue
        
        }
      
    }
    else{
      
      #True Negative
      if(expected[i]==0){
        results$TN <- results$TP + 1
        
      #False Negative
      }else{
        results$FN <- results$FN + 1
        costToReplaceWithModel <- costToReplaceWithModel + acquisitionCost
        costToReplaceWithoutModel <- costToReplaceWithoutModel + acquisitionCost
        lostRevenueWithoutModel <- lostRevenueWithoutModel+customerValue
        
        lostRevenueWithModel <- lostRevenueWithModel+customerValue
      }
    }
  }
  
  results$mispentEnticements <- wronglyEnticedCost
  results$correctEnticements <- correctlyEnticedCost
  results$totalEnticementSpend <- wronglyEnticedCost + correctlyEnticedCost
  results$lostRevenueWithModel <- lostRevenueWithModel
  results$lostRevenueWithoutModel <- lostRevenueWithoutModel
  results$costToReplaceWithModel <- costToReplaceWithModel
  results$costToReplaceWithoutModel <- costToReplaceWithoutModel
  results$totalSpendWithoutModel <- results$costToReplaceWithoutModel
  results$totalSpendWithModel <- results$costToReplaceWithModel+results$totalEnticementSpend
  results$ROI <- results$costToReplaceWithoutModel/results$totalSpendWithModel
  return(results)
}



calculateModelROI<-function(acquisitionCost, enticementPercent,minEnticementThreshold){
  #We need a scaled dataset for training and an unscaled dataset for cost calculations
  scaledDataset <-  mars_GetPreprocessedDataset(scaleflag=TRUE, printflag=FALSE)
  unscaledDataset <-  mars_GetPreprocessedDataset(scaleflag=FALSE, printflag=FALSE)

  ##Split dataset into train/validation set and test set
  trainSamples <- round(nrow(scaledDataset)*0.9)
  scaledTrain <- scaledDataset[1:trainSamples,]
  unscaledTrain <- unscaledDataset[1:trainSamples,]
  
  test <- scaledDataset[(trainSamples+1):nrow(scaledDataset),]
  unscaledTest <-  unscaledDataset[(trainSamples+1):nrow(scaledDataset),]
  test_expected <- test[,OUTPUT_FIELD]
  test_monthlyCharges <- unscaledTest[,"MonthlyCharges"]
  

  # ##Train an ensemble model on the full train set and output final test measures
  ensembleModel <- createEnsembleModel(scaledTrain)
  ensemblePredictions <- ensemblePredictMean(ensembleModel$lrModel, ensembleModel$rfModel, ensembleModel$nnModel,test)
  ensembleTestResults<-NdetermineThreshold(ensemblePredictions,test_expected,plot=FALSE)
  NprintMeasures(ensembleTestResults, "Ensemble model final test evaluation")

  threshold <- NcalculateThreshold(ensemblePredictions, test_expected)
  results <- evaluateModel(ensemblePredictions, test_expected, threshold, test_monthlyCharges, acquisitionCost, enticementPercent,minEnticementThreshold)
  print(results)
  NprintMeasures(results, "Ensemble model test results")
  
  results <-  kfold(scaledTrain, 5, ensembleROI, monthlyCharges=unscaledTrain[,'MonthlyCharges'], acquisitionCost=acquisitionCost, enticementPercent = enticementPercent,minEnticementThreshold=minEnticementThreshold)
  print(results)
  NprintMeasures(results, "Ensemble model validation results")
  

  
  return(results)
  
} #endof main()


plot



# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)
# ************************************************
# This is where R starts execution


#Load additional R script files 
source("functions/mars/data_pre_processing_pipeline.R")
source("functions/mars/data_pre_processing_functions.R")
source("functions/nick/4labfunctions.R")
source("functions/nick/lab4DataPrepNew.R")
source("functions/nick/lab3DataPrep.R")
source("functions/mars/utility_functions.R")
source("functions/mars/ensemble.R")


calculateModelROI(750,0.1,25)


