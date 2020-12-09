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
# INPUT     :   string            - modelName     - name of model to output to file
#           :   model             - Trained model object
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
# INPUT     :   string                 - modelName      - File name containing trained model.

#
# OUTPUT    :   object                 - model          - trained model
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
# INPUT     :   data frame                    - dataset         - the data frame
#               vector                        - fieldNames      - fields to retain
#
# OUTPUT    :   data frame                    - sub             - dataset with only fieldName columns retained
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
# INPUT     :   data frame                    - dataset         - the data frame
#               vector                        - fieldNames      - fields to drop
#
# OUTPUT    :   data frame                    - sub             - dataset with fieldNames dropped
#
# ************************************************
dropFields <- function(dataset,fieldNames){
  sub <- dataset[ , !(names(dataset) %in% fieldNames)]
  return(sub)
}


# ************************************************
# Name      :   kfold() :
# Purpose   :   Generic k-fold implementation, supports passing through parameters
#
# INPUT     :   data frame                             - dataset          - the dataset
#               integer                                - k                - number of folds
#               function                               - FUN              - model evaluation function
#
# OUTPUT    :   list                                   - avgs             - model performance measure averages
#
# ************************************************
kfold <-  function(dataset, k, FUN,...){
  print(paste("Running K-Fold cross validation with", k, "folds"))

  #Apply folds to records
  dataset <- PREPROCESSING_stratDataset(dataset, k)
  
  #Save the randomisation order
  randomisationOrder <- order(runif(nrow(dataset)))
  
  #Shuffle the dataset
  dataset <- dataset[randomisationOrder,]

  #Create the aggregate results dataframe
  results <-  data.frame()
  
  #Iterate over each fold from 1 to k
  for(i in 1:k){
    print(paste("Evaluating fold", i, "/", k))
    train <-  subset(dataset, (dataset$foldId!=i))
    test <-  dataset[dataset$foldId == i,]
    
    #We need to drop the fold id as we don't want our models training on this
    train <-  dropFields(train, c("foldId"))
    test <-  dropFields(test, c("foldId"))
    
    #Evaluate the model using the model function, train, test, and any other parameters the function might need
    result <-  FUN(train,test, ...)

    #Add the results to the results list
    results <-rbind(results, data.frame(result))
  }
  
  #Average the results, rounding the values to format nicely
  avgs <-  colMeans(results)
  avgs[1:4] <-  as.integer(round(avgs[1:4]))
  avgs[5:length(avgs)] <-  round(avgs[5:length(avgs)], digits=2)
  
  return(avgs)
}

