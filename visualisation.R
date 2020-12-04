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

#values for changing the colour of the bar graphs
yesColour <- "#ff6961"
noColour <- "#83F283"



#reads the dataset
dataset <- read.csv("telco-data.csv")

#turns the values in the seniorCitizen field from 1 and 0 into yes and no, for the ease of reading the graphs
dataset$SeniorCitizen <- factor(dataset$SeniorCitizen,levels = c(1,0), labels = c("Yes", "No"))




#bar graph to compare gender,SeniorCitizen, Partner, Dependents with churn
barGraph1 <- plot_grid(ggplot(dataset, aes(x=gender, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Gender", y="Churn Count", title = "Gender Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=SeniorCitizen, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Senior Citizen", y="Churn Count", title = "Senior Citizen Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.5) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=Partner, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Partner", y="Churn Count", title = "Partner Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=Dependents, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Dependents", y="Churn Count", title = "Dependents Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.3)+ scale_fill_manual(values=c(noColour, yesColour)))
print(barGraph1)


#bar graph to compare PhoneService and MultipleLines with churn
barGraph2 <- plot_grid(ggplot(dataset, aes(x=PhoneService, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Phone Service", y="Churn Count", title = "Phone Service Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.4) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=MultipleLines, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Multipe Phone Lines", y="Churn Count", title = "Multipe Lines Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)))
print(barGraph2)


#bar graph to compare InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport with churn
barGraph3 <- plot_grid(ggplot(dataset, aes(x=InternetService, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Internet Service", y="Churn Count", title = "Internet Service Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.1) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=OnlineSecurity, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Online Security", y="Churn Count", title = "Online Security Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.1) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=OnlineBackup, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Online Backup", y="Churn Count", title = "Online Backup Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.1) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=DeviceProtection, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Device Protection", y="Churn Count", title = "Device Protection Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.1) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=TechSupport, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Tech Support", y="Churn Count", title = "Tech Support Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.1) + scale_fill_manual(values=c(noColour, yesColour)))
print(barGraph3)


#bar graph to compare StreamingTV, StreamingMovies with churn
barGraph4 <- plot_grid(ggplot(dataset, aes(x=StreamingTV, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Streaming TV", y="Churn Count", title = "Streaming TV Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=StreamingMovies, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Streaming Movies", y="Churn Count", title = "Streaming Movies Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)))
print(barGraph4)


#bar graph to compare Contract, PaperlessBilling, PaymentMethod with churn
barGraph5 <- plot_grid(ggplot(dataset, aes(x=Contract, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Contract", y="Churn Count", title = "Contract Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.2) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=PaperlessBilling, fill=Churn)) + geom_bar(position="dodge") + labs(x= "Paperless Billing", y="Churn Count", title = "Paperless Billing Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)),
    ggplot(dataset, aes(x=PaymentMethod, fill=Churn)) + geom_bar(position="dodge") + scale_x_discrete(guide = guide_axis(n.dodge=3)) + labs(x= "Payment Method", y="Churn Count", title = "Payment Method Churn Rates") + geom_text(stat="count",aes(label=..count..), position=position_dodge(width=0.9), vjust=1.6) + scale_fill_manual(values=c(noColour, yesColour)))
print(barGraph5)


#below creates graphs for each field with numeric data
#bar graph to compare tenure with churn
histogram1 <- ggplot(dataset, aes(x=tenure, fill=Churn)) + geom_histogram(binwidth =4, position="dodge") + labs(x= "Tenure", y="Churn Count", title = "Tenure Churn Distribution") + scale_fill_manual(values=c(noColour, yesColour))
print(histogram1)
#bar graph to compare MonthlyCharges with churn
histogram2 <- ggplot(dataset, aes(x=MonthlyCharges, fill=Churn)) + geom_histogram(binwidth = 5, position="dodge") + labs(x= "Monthly Charges", y="Churn Count", title = "Monthly Charges Churn Distribution") + scale_fill_manual(values=c(noColour, yesColour))
print(histogram2)
#bar graph to compare TotalCharges with churn
histogram3 <- ggplot(dataset, aes(x=TotalCharges, fill=Churn)) + geom_histogram(binwidth = 250, position="dodge") + labs(x= "Total Charges", y="Churn Count", title = "Total Charges Churn Distribution") + scale_fill_manual(values=c(noColour, yesColour))
print(histogram3)







