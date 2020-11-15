# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  Mars Group
# 15 NOVEMBER 2020
# UPDATE
# 1.00      15/11/2020    Ryan    Initial Version
# ************************************************
#  clears all objects in "global environment", console and plots tab
rm(list=ls())
cat("\014")
graphics.off()

#install.packages("tidyverse")
library(ggplot2)

#reads the dataset
dataset <- read.csv("telco-data.csv")

#turns the values in the seniorCitizen field from 1 and 0 into yes and no, for the ease of readin the graphs
dataset$SeniorCitizen <- factor(dataset$SeniorCitizen,levels = c(1,0), labels = c("Yes", "No"))




#bar graph to compare gender with churn
barGraph1 <- ggplot(dataset, aes(x=gender, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare seniorCitizen with churn
barGraph2 <- ggplot(dataset, aes(x=SeniorCitizen, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare Partner with churn
barGraph3 <- ggplot(dataset, aes(x=Partner, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare Dependents with churn
barGraph4 <- ggplot(dataset, aes(x=Dependents, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare PhoneService with churn
barGraph5 <- ggplot(dataset, aes(x=PhoneService, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare MultipleLines with churn
barGraph6 <- ggplot(dataset, aes(x=MultipleLines, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare InternetService with churn
barGraph7 <- ggplot(dataset, aes(x=InternetService, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare OnlineSecurity with churn
barGraph8 <- ggplot(dataset, aes(x=OnlineSecurity, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare OnlineBackup with churn
barGraph9 <- ggplot(dataset, aes(x=OnlineBackup, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare DeviceProtection with churn
barGraph10 <- ggplot(dataset, aes(x=DeviceProtection, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare TechSupport with churn
barGraph11 <- ggplot(dataset, aes(x=TechSupport, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare StreamingTV with churn
barGraph12 <- ggplot(dataset, aes(x=StreamingTV, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare StreamingMovies with churn
barGraph13 <- ggplot(dataset, aes(x=StreamingMovies, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare Contract with churn
barGraph14 <- ggplot(dataset, aes(x=Contract, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare PaperlessBilling with churn
barGraph15 <- ggplot(dataset, aes(x=PaperlessBilling, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")
#bar graph to compare PaymentMethod with churn
barGraph16 <- ggplot(dataset, aes(x=PaymentMethod, fill=Churn, beside=TRUE)) + geom_bar(position="dodge")

#prints the above graphs
for (i in 1:16){
  barNum <- eval(parse(text = paste0("barGraph", i)))
  print(barNum)
}

#below creates graphs for each field with numeric data
#bar graph to compare tenure with churn
histogram1 <- ggplot(dataset, aes(x=tenure, fill=Churn, beside=TRUE)) + geom_histogram(binwidth =4, position="dodge")
print(histogram1)
#bar graph to compare MonthlyCharges with churn
histogram2 <- ggplot(dataset, aes(x=MonthlyCharges, fill=Churn, beside=TRUE)) + geom_histogram(binwidth = 5, position="dodge")
print(histogram2)
#bar graph to compare TotalCharges with churn
histogram3 <- ggplot(dataset, aes(x=TotalCharges, fill=Churn, beside=TRUE)) + geom_density(alpha= 0.5, position="dodge")
print(histogram3)







