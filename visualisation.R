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
library(cowplot)

#reads the dataset
dataset <- read.csv("telco-data.csv")

#turns the values in the seniorCitizen field from 1 and 0 into yes and no, for the ease of readin the graphs
dataset$SeniorCitizen <- factor(dataset$SeniorCitizen,levels = c(1,0), labels = c("Yes", "No"))




#bar graph to compare gender,SeniorCitizen, Partner, Dependents with churn
barGraph1 <- plot_grid(ggplot(dataset, aes(x=gender, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=SeniorCitizen, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=Partner, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=Dependents, fill=Churn)) + geom_bar(position="dodge"))
print(barGraph1)


#bar graph to compare PhoneService and MultipleLines with churn
barGraph2 <- plot_grid(ggplot(dataset, aes(x=PhoneService, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=MultipleLines, fill=Churn)) + geom_bar(position="dodge"))
print(barGraph2)


#bar graph to compare InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport with churn
barGraph3 <- plot_grid(ggplot(dataset, aes(x=InternetService, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=OnlineSecurity, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=OnlineBackup, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=DeviceProtection, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=TechSupport, fill=Churn)) + geom_bar(position="dodge"))
print(barGraph3)


#bar graph to compare StreamingTV, StreamingMovies with churn
barGraph4 <- plot_grid(ggplot(dataset, aes(x=StreamingTV, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=StreamingMovies, fill=Churn)) + geom_bar(position="dodge"))
print(barGraph4)


#bar graph to compare Contract, PaperlessBilling, PaymentMethod with churn
barGraph5 <- plot_grid(ggplot(dataset, aes(x=Contract, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=PaperlessBilling, fill=Churn)) + geom_bar(position="dodge"),
    ggplot(dataset, aes(x=PaymentMethod, fill=Churn)) + geom_bar(position="dodge") + scale_x_discrete(guide = guide_axis(n.dodge=3)))
print(barGraph5)



#below creates graphs for each field with numeric data
#bar graph to compare tenure with churn
histogram1 <- ggplot(dataset, aes(x=tenure, fill=Churn)) + geom_histogram(binwidth =4, position="dodge")
print(histogram1)
#bar graph to compare MonthlyCharges with churn
histogram2 <- ggplot(dataset, aes(x=MonthlyCharges, fill=Churn)) + geom_histogram(binwidth = 5, position="dodge")
print(histogram2)
#bar graph to compare TotalCharges with churn
histogram3 <- ggplot(dataset, aes(x=TotalCharges, fill=Churn)) + geom_histogram(binwidth = 250, position="dodge")
print(histogram3)







