# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   
#   DATE:     04 DECEMBER 2020
#   VERSION:  V1.02
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      17/11/2020    Chris Endacott    Initial Version
#   1.01      18/11/2020    Chris Endacott    Bug fixes
#   1.02      4/12/2020    Chris Endacott    Implemented ROI calculation workflow with ensemble
# ************************************************


# ************************************************
# Name      :   ensemble() :
# Purpose   :   Create, train and evaluate an ensemble model
#
# INPUT     :   data frame    - train     - the train dataset
#           :   data frame    - test      - the test dataset
#
# OUTPUT    :   data frame    - results   - model performance metrics
#
# ************************************************
ensemble <- function(train,test,...){
  
  ensembleModel <- createEnsembleModel(train)
  
  #Can optionally switch to mean based predictions
  #ensemblePredictions <-  ensemblePredictMean(ensembleModel$lrModel, ensembleModel$rfModel, ensembleModel$nnModel, test)
  ensemblePredictions <-  ensemblePredictVote(ensembleModel, test)
  
  test_expected <- test[,OUTPUT_FIELD]
  results<-NdetermineThreshold(ensemblePredictions, test_expected, plot=FALSE, title="Ensemble results")
  
  return(results)
}

# ************************************************
# Name      :   ensembleROI() :
# Purpose   :   Create, train and evaluate an ensemble model with respect to business metrics
#
# INPUT     :   data frame      - train                    - the train dataset
#           :   data frame      - test                     - the test dataset
#           :   double          - threshold                - the threshold for classifying predictions
#           :   vector double   - monthlyCharges           - the customer original value monthly charges
#           :   double          - acquisitionCost          - the cost to acquire a new subscriber
#           :   double          - enticementPercent        - the percentage enticement to retain a subscriber
#           :   double          - minEnticementThreshold   - the minimum monthlycharges we will offer enticements to
#
# OUTPUT    :   data frame      - results                  - evaluated business metrics
#
# ************************************************
ensembleROI <- function(train,test,threshold, monthlyCharges, acquisitionCost, enticementPercent, minEnticementThreshold,...){
  
  #Get the raw monthly charges for each test row
  testRowIds <- as.numeric(rownames(test))
  testMonthlyCharges <- c()
  for(rowId in testRowIds){
    testMonthlyCharges <- append(testMonthlyCharges,monthlyCharges[rowId])
  }
  
  ensembleModel <- createEnsembleModel(train)
  
  #Optionally use mean predictions
  #ensemblePredictions <-  ensemblePredictMean(ensembleModel, test)
  ensemblePredictions <-  ensemblePredictVote(ensembleModel, test)
  
  test_expected <- test[,OUTPUT_FIELD]
  threshold <- NcalculateThreshold(ensemblePredictions, test_expected)
  results <- evaluateModel(ensemblePredictions, test_expected, threshold, testMonthlyCharges, acquisitionCost, enticementPercent,minEnticementThreshold)
  
  return(results)
}


# ************************************************
# Name      :   createEnsembleModel() :
# Purpose   :   Creates a custom ensembleModel 'Class' 
#
# INPUT     :   data frame    - train          - the dataset to train the model with
#
# OUTPUT    :   list object   - ensembleModel  - The trained ensemble model
#
# ************************************************
createEnsembleModel <- function(train){
  
  #Train a logistic regression model
  logisticRegressionModel <-  createLogisticRegressionModel(train)
  print("Logistic Regression model trained")

  #Train a random forest model
  randomForestModel <-  createRandomForestModel(train)
  print("Random Forest model trained")
  
  #Train a neural network model
  neuralNetworkModel <-  createNeuralNetworkModel(train)
  print("Neural Network model trained")
  
  #Create the ensemble 'model'. R is quite restrictive in creating classes, so our model here effectively is just a list of the base models.
  ensembleModel <-  list("lrModel" = logisticRegressionModel, "rfModel" = randomForestModel, "nnModel" = neuralNetworkModel)
  
  return(ensembleModel)
  
}



# ************************************************
# Name      :   ensemblePredictVote() :
# Purpose   :   Calculates class predictions using for the custom 'EnsembleModel' class using the 'Vote' method
#
# INPUT     :   object           - ensembleModel        - the ensemble model
#           :   data frame       - test                 - the dataset to predict
#
# OUTPUT    :   vector double    - ensemblePredictions  - the class predictions for the input dataset
#
# ************************************************
ensemblePredictVote <- function(ensembleModel, test){
  #Define the targets
  test_expected <- test[,OUTPUT_FIELD]
  
  #Get the logistic regression prediction and threshold it into a 1 or 0 binary vote
  logrPredictions <- lrPredict(ensembleModel$lrModel, test)
  logrThreshold <- NdetermineThreshold(logrPredictions, test_expected)$threshold
  logrVotes<-ifelse(logrPredictions<logrThreshold,0,1)
  
  #Get the random forest prediction and threshold it into a 1 or 0 binary vote
  rfPredictions <- rfPredict(ensembleModel$rfModel, test)
  rfThreshold <- NdetermineThreshold(rfPredictions, test_expected)$threshold
  rfVotes<-ifelse(rfPredictions<rfThreshold,0,1)
  
  #Get the neural network prediction and threshold it into a 1 or 0 binary vote
  nnPredictions <-  nnPredict(ensembleModel$nnModel,test)
  nnThreshold <- NdetermineThreshold(nnPredictions, test_expected)$threshold
  nnVotes <-ifelse(nnPredictions<nnThreshold,0,1)
  
  #Initialise the vector for storing our predictions
  ensemblePredictions <- vector()
  
  for(i in 1:nrow(test)){
    #Majority vote needs more than 1
    if(sum(nnVotes[i],rfVotes[i], logrVotes[i])>1.5){
      ensemblePredictions <-  append(ensemblePredictions, 1)
    }else{
      ensemblePredictions <-  append(ensemblePredictions, 0)
      
    }
    
  }
  return(ensemblePredictions)
}

# ************************************************
# Name      :   ensemblePredictMean() :
# Purpose   :   Calculates class predictions using for the custom 'EnsembleModel' class using the 'Mean average' method
#
# INPUT     :   object           - ensembleModel        - the ensemble model
#           :   data frame       - test                 - the dataset to predict
#
# OUTPUT    :   vector double    - ensemblePredictions  - the class predictions for the input dataset
#
# ************************************************
ensemblePredictMean <- function(ensembleModel, test){
  #Define the targets
  test_expected <- test[,OUTPUT_FIELD]

  #Get the raw predictions for each model
  logrPredictions <- lrPredict(ensembleModel$lrModel, test)
  rfPredictions <- rfPredict(ensembleModel$rfModel, test)
  nnPredictions <-  nnPredict(ensembleModel$nnModel,test)

  #Initialise the vector for storing our predictions
  ensemblePredictions <- vector()
  
  #Average the mean prediction as each ensemble prediction
  for(i in 1:nrow(test)){
    meanPrediction <-  (logrPredictions[i]+rfPredictions[i]+nnPredictions[i])/3
    ensemblePredictions <-  append(ensemblePredictions,meanPrediction)
    
  }

  return(ensemblePredictions)
}




# ************************************************
# Name      :   evaluateEnsembleModel() :
# Purpose   :   evaluate an ensemble model using k-Fold cross validation
#
# INPUT     :   data frame   - dataset    - the train dataset
#
# OUTPUT    :   data frame   - results    - the evaluation metrics
#
# ************************************************
evaluateEnsembleModel <- function(dataset){
  #Run k fold evaluation on the ensemble model
  results <-  kfold(dataset, 5, ensemble)
  
  return(results)
  
}
