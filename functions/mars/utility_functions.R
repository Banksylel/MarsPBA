# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   UTILITY FUNCTIONS
#
#   
#   DATE:     28 NOVEMBER 2020
#   VERSION:  V1.04
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.01      13/11/2020    Chris Endacott    Created generalisable k-fold
#   1.02      14/11/2020    Chris Endacott    Added function for dropping fields from dataset
#   1.03      17/11/2020    Chris Endacott    Implemented passing params through k-fold
#   1.04      28/11/2020    Chris Endacott    Added model saving


# ************************************************
# Name      :   saveModelToFile() :
# Purpose   :   Saved trained models to a file.
#
# INPUT     :   modelName     Filename to write model to
#           :   model         Trained model object
#
# OUTPUT    :   None
#
# ************************************************
saveModelToFile <- function(modelName, model){
  saveRDS(model, paste("Models/",modelName,".rda", sep=""))
}


# ************************************************
# Name      :   loadModelFromFile() :
# Purpose   :   Load trained model from a file
#
# INPUT     :   modelName     File containing trained model.

#
# OUTPUT    :   None
#
# ************************************************
loadModelFromFile <-function(modelName){
  model <- readRDS(paste("Models/",modelName,".rda",sep=""))
  return(model)
}


# ************************************************
# Name      :   keepFields() :
# Purpose   :   Strip columns other than fieldNames from dataset
#
# INPUT     :   dataset     dataset
#               fieldNames  Fields to retain
#
# OUTPUT    :   Input dataset with only fieldName columns retained
#
# ************************************************

keepFields <- function(dataset,fieldNames){
  sub <- dataset[ , (names(dataset) %in% fieldNames)]
  return(sub)
}


# ************************************************
# Name      :   dropFields() :
# Purpose   :   Strip fieldName columns from dataset
#
# INPUT     :   dataset     dataset
#               fieldNames  Fields to drop
#
# OUTPUT    :   Input dataset minus fieldName columns
#
# ************************************************
dropFields <- function(dataset,fieldNames){
  sub <- dataset[ , !(names(dataset) %in% fieldNames)]
  return(sub)
}


# ************************************************
# Name      :   kfold() :
# Purpose   :   Wrapper for k-fold validation
#
# INPUT     :   dataset     dataset
#               k           Number of folds
#               FUN         Model function
#
# OUTPUT    :   Model performance measure averages
#
# ************************************************

kfold <-  function(dataset, k, FUN,...){
  print(paste("Running K-Fold cross validation with", k, "folds"))

  dataset <- PREPROCESSING_stratDataset(dataset, k)
  randomisationOrder <- order(runif(nrow(dataset)))
  
  dataset <- dataset[randomisationOrder,]

  
  
  results <-  data.frame()
  
  for(i in 1:k){
    print(paste("Evaluating fold", i, "/", k))
    train <-  subset(dataset, (dataset$foldId!=i))
    test <-  dataset[dataset$foldId == i,]
    
    train <-  dropFields(train, c("foldId"))
    test <-  dropFields(test, c("foldId"))
    
    result <-  FUN(train,test, ...)

    results <-rbind(results, data.frame(result))
  }
  
  avgs <-  colMeans(results)
  avgs[1:4] <-  as.integer(round(avgs[1:4]))
  avgs[5:length(avgs)] <-  round(avgs[5:length(avgs)], digits=2)
  
  return(avgs)
}

print("Sourcing utility_functions.R ")