# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  Mars Group
# 14 NOVEMBER 2020
# UPDATE
# 1.00      14/11/2020    Brian   Initial Version
# 1.01      15/11/2020    Ryan    tweaks, and tidying
# ************************************************
# R Script For Data Visualization
#  clears all objects in "global environment", console and plots tab

# ************************************************
# Global Environment variables

DATASET_FILENAME  <- "telco-data.csv"          # Name of input dataset file


# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4


#install.packages("tidyverse")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
# User defined functions are next

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

visualiseDataset <- function(filename){
  dataset <- read.csv(filename)
  churn_data <- dataset%>%
    count(Churn) %>%
    mutate(per = n / sum(n),
           per_label = paste0(round(per*100), "%"))
  
  #turns the values in the seniorCitizen field from 1 and 0 into yes and no, for the ease of reading the graphs
  dataset$SeniorCitizen <- factor(dataset$SeniorCitizen,levels = c(1,0), labels = c("Yes", "No"))
  
  
  #Churn Distribution graph
  p <- ggplot(churn_data, aes(x= reorder(Churn, -per), y= per)) +
    geom_bar(stat= "identity", fill= "pink", color= "black")+
    geom_text(aes(label = per_label), vjust = -0.25) +
    labs( x= "Churn", y="Count", title = "Churn distribution") 
  print(p)
  #
  #**********************************************************
  #
  #Payment related visualizations
  #
  options(repr.plot.width = 18, repr.plot.height = 12)
  p <- plot_grid(ggplot(dataset, aes(x= PaymentMethod, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="Payment Method Churn rate"),
            ggplot(dataset, aes(x= PaperlessBilling, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="Paperless billing Churn rate"),
            ggplot(dataset, aes(x= Contract, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="Contract Churn rate"))
  print(p)
  
   #scale_x_discrete(labels = function(x) str_wrap(x, width = 20), align = "h")
  #**********************************************************
  #Streaming related visualization
  options(repr.plot.width = 18, repr.plot.height = 12)
  p <- plot_grid(ggplot(dataset, aes(x= StreamingMovies, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="StreamingMovies Churn rate"),
            ggplot(dataset, aes(x= StreamingTV, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="StreamingTV"))
  print(p)
  #***********************************************************
  #Service description visualisations
  options(repr.plot.width = 18, repr.plot.height = 12)
  p <- plot_grid(ggplot(dataset, aes(x= TechSupport, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="TechSupport Churn rate"),
            ggplot(dataset, aes(x= DeviceProtection, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="DeviceProtection"),
            ggplot(dataset, aes(x= OnlineBackup, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="OnlineBackup"),
            ggplot(dataset, aes(x= OnlineSecurity, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="OnlineSecurity"),
            ggplot(dataset, aes(x= InternetService, fill = Churn)) + theme_bw()+ geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+ labs(y= "Churn count", title="InternetService"))
  print(p)
  #************************************************************
  #Phonelines visualisations
  options(repr.plot.width = 18, repr.plot.height = 12)
  plot_grid(ggplot(dataset, aes(x= PhoneService, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="PhoneService Churn rate"),
            ggplot(dataset, aes(x= MultipleLines, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="MultipleLines Churn rate"))
  #************************************************************
  #Customer descriptor visualisations
  options(repr.plot.width = 18, repr.plot.height = 12)
  print(plot_grid(ggplot(dataset, aes(x= Dependents, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="Dependents Churn rate"),
                  ggplot(dataset, aes(x= Partner, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="Partner Churn rate"),
                  ggplot(dataset, aes(x= SeniorCitizen, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="SeniorCitizen Churn rate"),
                  ggplot(dataset, aes(x= gender, fill = Churn)) + theme_bw()+geom_bar(stat= "count", position="dodge")+ geom_text(stat="count", aes(label=..count..), vjust=2)+labs(y= "Churn count", title="Gender Churn rate")))
  #************************************************************
  #Tenure density plot
  p <- ggplot(dataset, aes(x= tenure, fill= Churn))+ geom_density(alpha= 0.5) +
    labs(title = "Tenure density")
  print(p)
  #************************************************************
  #Monthly Charges density
  p <- ggplot(dataset, aes(x= MonthlyCharges, fill= Churn))+ geom_density(alpha= 0.5) +
    labs(title = "Monthly Charges density")
  print(p)
  #************************************************************
  #Total Charges density
  p <- ggplot(dataset, aes(x= TotalCharges, fill= Churn))+ geom_density(alpha= 0.5) +
    labs(title = "TotalCharges density")
  print(p)
  #**********************************************************
}

