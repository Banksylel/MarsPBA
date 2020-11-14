# Logistic Regression

# Importing the dataset
dataset = read.csv('telco-data.csv')


# Encoding the target feature as factor
dataset$gender = factor(dataset$gender,
                         levels = c('Female', 'Male'),
                         labels = c(1,2))
dataset$Partner = factor(dataset$Partner,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
dataset$Dependents = factor(dataset$Dependents,
                         levels = c('No', 'Yes'),
                         labels = c(0, 1))
dataset$PhoneService = factor(dataset$PhoneService,
                         levels = c('No', 'Yes'),
                         labels = c(0, 1))
dataset$MultipleLines = factor(dataset$MultipleLines,
                           levels = c('No', 'Yes','No phone service'),
                           labels = c(0, 1, 2))
dataset$InternetService = factor(dataset$InternetService,
                                 levels = c('No','DSL', 'Fiber optic'),
                                 labels = c(0,1,2))
dataset$OnlineSecurity = factor(dataset$OnlineSecurity,
                                levels = c('No', 'Yes','No internet service'),
                                labels = c(0, 1, 2))
dataset$OnlineBackup = factor(dataset$OnlineBackup,
                         levels = c('No', 'Yes','No internet service'),
                         labels = c(0, 1, 2))
dataset$DeviceProtection = factor(dataset$DeviceProtection,
                              levels = c('No', 'Yes','No internet service'),
                              labels = c(0, 1, 2))
dataset$TechSupport = factor(dataset$TechSupport,
                              levels = c('No', 'Yes','No internet service'),
                              labels = c(0, 1, 2))
dataset$StreamingTV = factor(dataset$StreamingTV,
                              levels = c('No', 'Yes','No internet service'),
                              labels = c(0, 1, 2))
dataset$StreamingMovies = factor(dataset$StreamingMovies,
                                  levels = c('No', 'Yes','No internet service'),
                                  labels = c(0, 1, 2))
dataset$Contract = factor(dataset$Contract,
                               levels = c('Month-to-month','One year','Two year'),
                               labels = c(0, 1, 2))
dataset$PaperlessBilling = factor(dataset$PaperlessBilling,
                                 levels = c('No', 'Yes'),
                                 labels = c(0, 1))
dataset$PaymentMethod = factor(dataset$PaymentMethod,
                          levels = c('Mailed check','Bank transfer (automatic)','Electronic check','Credit card (automatic)'),
                          labels = c(1, 2, 3, 4))


dataset$Churn = factor(dataset$Churn,
                         levels = c('No', 'Yes'),
                         labels = c(0, 1))

dataset=dataset[-1]
# Splitting the dataset into the Training set and Test set

library(caTools)
set.seed(123)
split = sample.split(dataset$Churn, SplitRatio = 0.8)
train = subset(dataset, split == TRUE)
test = subset(dataset, split == FALSE)

# Fitting Logistic Regression to the Training set
logistic_reg = glm(formula = Churn ~ .,
                 family = binomial,
                 data = train)

# Predicting the Test set results
prob_pred = predict(logistic_reg, type = 'response', newdata = test[-20])
logistic_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test[, 20], logistic_pred > 0.5)
