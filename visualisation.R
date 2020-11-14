
rm(list=ls())

#install.packages("tidyverse")
library(ggplot2)






dataset <- read.csv("telco-data.csv")
   
dataset$Churn <- factor(dataset$Churn,
                        levels = c("Yes", "No"),
                         labels = c(1, 0))



p <- ggplot(dataset, aes(x=gender, fill=Churn))
p <- p + geom_bar()
print(p)

# ggplot(data = dataset, aes(x= tenure,y= SeniorCitizen)) + geom_point()
# ggplot(dataset, aes(x= gender)) + geom_bar()
# ggplot(dataset, aes(x= Partner, y= Churn)) + geom_point()
# ggplot(dataset, aes(x= TotalCharges, y=Churn)) + geom_point()
# ggplot(dataset, aes(x= PaymentMethod, fill = Churn))
# ggplot(dataset, aes(x=gender, fill=Churn))



# plotVisualisations <- function(dataset){
#   ggplot(dataset)
#   ggplot(data = dataset, aes(x= tenure,y= SeniorCitizen)) + geom_point()
#   ggplot(dataset, aes(x= gender)) + geom_bar()
#   ggplot(dataset, aes(x= Partner, y= Churn)) + geom_point()
#   ggplot(dataset, aes(x= TotalCharges, y=Churn)) + geom_point()
#   ggplot(dataset, aes(x= PaymentMethod, fill = Churn))
#   ggplot(dataset, aes(x=gender, fill=Churn))
#   
# }
# 
# main <- function(){
#   
#   churn <- read.csv("telco-data.csv")
#   
#   churn$Churn <- factor(churn$Churn,
#                         levels = c("Yes", "No"),
#                         labels = c(1, 0))
#   
#   plotVisualisations(churn)
#   print("hi")
#   
# }
# 
# 
# main()