churn <- read.csv("Churn.csv")

churn$Churn <- factor(churn$Churn,
                      levels = c("Yes", "No"),
                      labels = c(1, 0))


#install.packages("tidyverse")
library(ggplot2)
ggplot(churn)
ggplot(data = churn, aes(x= tenure,y= SeniorCitizen)) + geom_point()
ggplot(churn, aes(x= gender)) + geom_bar()
ggplot(churn, aes(x= Partner, y= Churn)) + geom_point()
ggplot(churn, aes(x= TotalCharges, y=Churn)) + geom_point()
ggplot(churn, aes(x= PaymentMethod, fill = Churn))


ggplot(churn, aes(x=gender, fill=Churn))