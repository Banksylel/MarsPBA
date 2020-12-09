# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   LOGISTIC REGRESSION FUNCTIONS
#
#   
#   DATE:     04 DECEMBER 2020
#   VERSION:  V1.02
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      03/12/2020    Chris Endacott    Initial Version
#   1.01      04/12/2020    Chris Endacott    Added visualisation
#   1.02      04/12/2020    Chris Endacott    Fixed ROI calc
# ************************************************

# ************************************************
# Name      :   calculateAverageCustomerLifetime() :
# Purpose   :   Calculate the average customer lifetime from the dataset file
#
# INPUT     :   none
#
# OUTPUT    :   double    - avgTenure  - the average tenure
#
# ************************************************
calculateAverageCustomerLifetime <- function(){
  rawDataset <- NreadDataset(DATASET_FILENAME)
  
  avgTenure <-  mean(rawDataset[,CUSTOMER_TENURE_FIELD])
  
  return(avgTenure)
  
}

# ************************************************
# Name      :   estimateCustomerValue() :
# Purpose   :   Estimate customer value using our assumption 'monthly charges * avg tenure'
#
# INPUT     :   double    - monthlyCharges  - the customer's monthly charges
#               double    - avgTenure       - the average customer tenure 
# 
# OUTPUT    :   double                      - the customer value
#
estimateCustomerValue <- function(monthlyCharges, avgTenure){
  return(monthlyCharges*avgTenure)
}



# ************************************************
# Name      :   evaluateMode() :
# Purpose   :   Calculates business metrics given a list of class predictions and a list of expected classes
#
# INPUT     :   vector double    - predicted                - the vector of class predictions
#           :   vector double    - expected                 - the vector of expected class labels
#           :   double           - threshold                - the threshold for classifying predictions
#           :   vector double    - monthlyCharges           - the customer original value monthly charges
#           :   double           - acquisitionCost          - the cost to acquire a new subscriber
#           :   double           - enticementPercent        - the percentage enticement to retain a subscriber
#           :   double           - minEnticementThreshold   - the minimum monthlycharges we will offer enticements to
#
# OUTPUT    :   vector double    - test_predicted           - the class predictions for the input dataset
#
# ************************************************
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
        results$TP <- results$TP + 1
        
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
        results$TN <- results$TN + 1
        
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
  results$totalSpendWithModel <- results$costToReplaceWithModel+results$totalEnticementSpend
  results$totalSpendWithoutModel <- results$costToReplaceWithoutModel
  
  netReturn <- results$totalSpendWithoutModel-results$totalSpendWithModel
  roi <- round((netReturn/results$totalEnticementSpend)*100,2)
  results$ROI <- roi
  return(results)
}


# ************************************************
# Name      :   evaluateEnsembleModelROI() :
# Purpose   :   Evaluate and plot the model business metrics and ROI given the dataset
#
# INPUT     :   data frame       - scaledDataset            - the scaled dataset
#           :   data frame       - unscaledDataset          - the unscaled dataset
#           :   double           - acquisitionCost          - the cost to acquire a new subscriber
#           :   double           - enticementPercent        - the percentage enticement to retain a subscriber
#           :   double           - minEnticementThreshold   - the minimum monthlycharges we will offer enticements to
#
# OUTPUT    :   data frame       - results                  - the evaluated business insight metrics for the model
#
# ************************************************
evaluateEnsembleModelROI<-function(scaledDataset, unscaledDataset,acquisitionCost, enticementPercent,minEnticementThreshold){
  ##Split dataset into train/validation set and test set
  trainSamples <- round(nrow(scaledDataset)*0.9)
  scaledTrain <- scaledDataset[1:trainSamples,]
  unscaledTrain <- unscaledDataset[1:trainSamples,]
  
  test <- scaledDataset[(trainSamples+1):nrow(scaledDataset),]
  unscaledTest <-  unscaledDataset[(trainSamples+1):nrow(scaledDataset),]
  test_expected <- test[,OUTPUT_FIELD]
  test_monthlyCharges <- unscaledTest[,MONTHLY_CHARGE_FIELD]
  

  # ##Train an ensemble model on the full train set and output final test measures
  ensembleModel <- createEnsembleModel(scaledTrain)
  ensemblePredictions <- ensemblePredictVote(ensembleModel,test)
  ensembleTestResults<-NdetermineThreshold(ensemblePredictions,test_expected,plot=FALSE)
  NprintMeasures(ensembleTestResults, "Ensemble model final test evaluation")

  threshold <- NcalculateThreshold(ensemblePredictions, test_expected)
  results <- evaluateModel(ensemblePredictions, test_expected, threshold, test_monthlyCharges, acquisitionCost, enticementPercent,minEnticementThreshold)
  print(results)
  NprintMeasures(results, "Ensemble model test results")
  plotResults(results, round=FALSE)
  
  
  results <-  kfold(scaledTrain, 5, ensembleROI, monthlyCharges=unscaledTrain[,MONTHLY_CHARGE_FIELD], acquisitionCost=acquisitionCost, enticementPercent = enticementPercent,minEnticementThreshold=minEnticementThreshold)
  print(results)
  NprintMeasures(results, "Ensemble model validation results")
  plotResults(results, round=FALSE)
  

  
  return(results)
  
} #endof main()



# ************************************************
# Name      :   plotResults() :
# Purpose   :   Plot calculated business results
#
# INPUT     :   data frame       - results            - the evaluated business metrics data frame
#           :   boolean          - round              - true to round the figures in the plot
#
# OUTPUT    :   vector double    - test_predicted     - the class predictions for the input dataset
#
# ************************************************
plotResults <- function(results, round=TRUE){
  
  totalEnticementSpend <-  as.vector(as.numeric(results["totalEnticementSpend"]))
  totalEnticementSpend <- append(totalEnticementSpend,0)
  
  costToReplace <- as.vector(as.numeric(results["costToReplaceWithModel"]))
  costToReplace <- append(costToReplace,as.numeric(results["costToReplaceWithoutModel"]))
  
  totalSpend <- as.vector(as.numeric(results["totalSpendWithModel"]))
  totalSpend <- append(totalSpend,as.numeric(results["totalSpendWithoutModel"]))
  
  if(round){
    totalEnticementSpend <- round(totalEnticementSpend,-3)
    costToReplace <- round(costToReplace,-3)
    totalSpend <- round(totalSpend,-3)
    
  }
  
  df = melt(data.frame("Total Enticement Spend"=totalEnticementSpend, "Cost To Replace Lost Subscribers"=costToReplace, "Total Spend"=totalSpend, 
                       experiment=c("With Model","Without Model")),
            variable_name="Metric")
  
  p <- ggplot(df, aes(experiment, value, fill=Metric)) + 
    geom_bar(position="dodge", stat='identity')+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    geom_text(aes(label=dollarValueToFormattedString(value)), position=position_dodge(width=0.9), vjust=-0.25)
  
  
  print(p)
}

# ************************************************
# Name      :   dollarValueToFormattedString() :
# Purpose   :   Turn a double into a string into the format $xx,xxx.xx
#
# INPUT     :   double       - value            - the dollar amount
#
# OUTPUT    :   string       - value            - the dollar value as a formated string
#
# ************************************************
dollarValueToFormattedString <- function(value){
  value <-  round(value,digits=0)
  value <- format(value,big.mark=",",scientific=FALSE)
  value <- trimws(value, which = "both", whitespace = " ")
  value <- paste("$", value, sep="")
  return(value)
}


