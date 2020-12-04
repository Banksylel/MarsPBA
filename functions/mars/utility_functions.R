# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   UTILITY FUNCTIONS
#
#   
#   DATE:     11 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version


saveModelToFile <- function(modelName, model){
  saveRDS(model, paste("Models/",modelName,".rda", sep=""))
}

loadModelFromFile <-function(modelName){
  model <- readRDS(paste("Models/",modelName,".rda",sep=""))
  return(model)
}


keepFields <- function(dataset,fieldNames){
  sub <- dataset[ , (names(dataset) %in% fieldNames)]
  return(sub)
}

dropFields <- function(dataset,fieldNames){
  sub <- dataset[ , !(names(dataset) %in% fieldNames)]
  return(sub)
}


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