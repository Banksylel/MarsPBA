# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   LOGISTIC REGRESSION FUNCTIONS
#
#   
#   DATE:     16 NOVEMBER 2020
#   VERSION:  V1.02
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.01      15/11/2020    Chris Jennings    Adopt k-folds
#   1.02      16/11/2020    Chris Jennings    Fixed bugs
# ************************************************

# ************************************************
# Name      :   getLRClassifications() :
# Purpose   :   Determine "measures" when using optimal threshold
#
# INPUT     :   object                   - trainedModel        - the trained model
#           :   data frame               - testDataset         - the test dataset
#           :   string                   - title               - Optional title      
#           :   Boolean                  - plot                - Enable plots
#
# OUTPUT    :   list                     - measures            - model performance metrics
#
# ************************************************
getLRClassifications<-function(trainedModel,
                                 testDataset,
                                 title="",
                                 plot=FALSE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Get probabilities of being class 1 from the classifier
  test_predictedProbs<-predict(trainedModel,test_inputs, type="response")


  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)

  return(measures)
} #endof getLRClassifications()


# ************************************************
# Name      :   logisticRegression() :
# Purpose   :   Train logistic regression model
#
# INPUT     :   data frame        - training_data        - the train data
#           :   data frame        - testing_data         - the test data
#           :   string            - formular             - the formula for the logistic regression
#           :   Boolean           - plot                 - Enable plots
#
# OUTPUT    :   list              - measures             - Model performance metrics
#
# ************************************************
logisticRegression <- function(training_data,testing_data, formular=formular, plot=TRUE, ...){

  # Train model with reduced feature list
  logr<-stats::glm(formular,data=training_data,family=quasibinomial)

  
  # ************************************************
  # Use the trained model with the test dataset
  measures<-getLRClassifications(trainedModel = logr,
                                   testDataset = testing_data,
                                   title=myTitle,
                                   plot=FALSE)
  
  
  return(measures)
} #endof logisticRegression()



# ************************************************
# Name      :   reduceFeatures() :
# Purpose   :   Train logistic regression model on entire dataset to
#           :   determine feature importance.
#               Remove non-contributing features and generate model formula.
#
# INPUT     :   data frame                                   - dataset        - the dataset
#
# OUTPUT    :   string                                       - formular       - Model formula with reduced feature set.
#
# ************************************************
reduceFeatures<-function(dataset) {
  
  # Determine importance of features
  formular<-myModelFormula(dataset = dataset, fieldNameOutput = OUTPUT_FIELD)
  logr<-stats::glm(formular,data=dataset,family=quasibinomial)
  
  # Exclude features of no importance - avoids rank deficiency issues
  importance<-as.data.frame(caret::varImp(logr, scale = TRUE))
  features<-data.frame(gsub("[[:blank:]]+", "", row.names(importance)), importance$Overall)
  colnames(features)<-c("Feature", "Overall")
  features<-features[order(-importance$Overall),]

  # Reconstruct model formula
  formular<-paste(OUTPUT_FIELD, "~", paste(features$Feature, collapse = "+"))
  
  return(formular)
  
}

# ************************************************
# Name      :   retention() :
# Purpose   :   Cost of retention plot - discount vs churn
#
# INPUT     :   object        - trainedModel       - the trained model
#           :   double        - threshold          - the classification threshold
#           :   data frame    - dataset            - the dataset
#           :   string        - title              - Title for plot       
#
# OUTPUT    :   None
#
# ************************************************
retention<-function(trainedModel, threshold, dataset, title){

  positionClassOutput=which(names(dataset)==OUTPUT_FIELD)

  # Dataframe with with just input fields
  inputs<-dataset[-positionClassOutput]

  # Separate monthly charge column
  monthlyCharge<-dataset$MonthlyCharges
  discountFactorVec<-vector()
  churnRateVec<-vector()
  
  # It's a straight line for linear models so can plot with two endpoints only
  # Include range for x values for possible later use with non-linear models.
  for (discountFactor in seq(from=0, to=100, by=10)) 
  {
    discMonthlyCharge<-(100 - discountFactor) * monthlyCharge / 100
    inputs$MonthlyCharges<-discMonthlyCharge
    probs<-predict(trainedModel,inputs, type="response")
    churnRateVec<-c(churnRateVec, 100 * sum(probs>threshold) / length(probs))
    discountFactorVec<-c(discountFactorVec,discountFactor)
  }
  xRange<-range(0,100,10)
  yRange<-range(0,100)
  grid(nx = 10, ny = 10, col = "lightgray", lty = "dotted",
       lwd = par("lwd"))
  plot(discountFactorVec,  churnRateVec, 
       axes=TRUE, 
       lwd = 5,
       col = "#69b3a2",
       main = title,
       xlim = xRange, 
       ylim = yRange, 
       panel.first = grid(),
       type = "l", 
       xlab = "Discount %", 
       ylab = "Churn rate %")

}

# ************************************************
# Name      :   lrPredict() :
# Purpose   :   Calculates class predictions using a logistic regression model
#
# INPUT     :   object           - model                - the trained logistic regression model
#           :   data frame       - test                 - the dataset to predict
#
# OUTPUT    :   vector double    - test_predictedProbs  - the class predictions for the input dataset
#
# ************************************************
lrPredict <- function(model, test){
  positionClassOutput=which(names(test)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]
  
  # Get probabilities of being class 1 from the classifier
  test_predictedProbs<-predict(model,test_inputs, type="response")
  
  return(test_predictedProbs)
}

# ************************************************
# Name      :   createLogisticRegressionModel() :
# Purpose   :   Train logistic regression model
#
# INPUT     :   data frame      - dataset      - Train dataset
#           :   boolean         - print        - output intermediate values?
# 
# OUTPUT    :   Trained model
#
# ************************************************
createLogisticRegressionModel <- function(dataset, print=FALSE){
  # Remove redundant features from model
  formular<-reduceFeatures(dataset)
  logr<-stats::glm(formular,data=dataset,family=quasibinomial)
  
  return(logr)
  
} 


# ************************************************
# Name      :   evaluateLogisticRegressionModel() :
# Purpose   :   Evaluate logistic regression model
#
# INPUT     :   data frame                    - dataset      - Train dataset
#
# OUTPUT    :   list                          - results      - evaluation metrics
#
# ************************************************

evaluateLogisticRegressionModel<-function(dataset){
  
  
  # Remove redundant features from model
  formular<-reduceFeatures(dataset)
  
  # Run k-folds validation
  results <-  kfold(dataset, 5, logisticRegression, formular)

  # # Print k-folds measures means
  # NprintMeasures(results)
  
  # # Train new model for further analysis
   logr<-stats::glm(formular,data=dataset,family=quasibinomial)
  # 
  # # Plot effect of discounts
  # threshold<-results["threshold"]
  # retention(trainedModel = logr, threshold = threshold, 
  #           dataset = dataset, 
  #           title = "Monthly Charge Discount vs Churn Rate")
  
  # Plot feature importance chart
  importance<-as.data.frame(caret::varImp(logr, scale = TRUE))
  row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
  par(mar=c(3,12,3,2)+.1)
  barplot(t(importance[order(-importance$Overall),,drop=FALSE]),
          las=1, 
          border = NA, 
          cex.names = 0.8, 
          horiz = TRUE,
          col="#69b3a2",
          xlim = c(0,10),
          main = "Logistic regression feature importance")
  

  return(results)
} 





