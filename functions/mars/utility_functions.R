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
  dataset <- dataset[order(runif(nrow(dataset))),]
  
  results <-  data.frame()
  
  for(i in 1:k){
    train <-  subset(dataset, (dataset$foldId!=i))
    test <-  dataset[dataset$foldId == i,]
    
    train <-  dropFields(train, c("foldId"))
    test <-  dropFields(test, c("foldId"))
    
    result <-  FUN(train,test, plot=TRUE)
    
    results <-rbind(results, data.frame(result))
  }
  
  avgs <-  colMeans(results)
  avgs[1:4] <-  as.integer(round(avgs[1:4]))
  
  return(avgs)
}

print("Sourcing utility_functions.R ")