# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  Mars Group
# 15 NOVEMBER 2020
# UPDATE
# 1.00      14/11/2020    Brian   Initial Version
# 1.01      15/11/2020    Ryan    tweaks, and tidying
# ************************************************


# ************************************************
# Name      :   visualiseDataset() :
# Purpose   :   Perform visualisation on the input dataset
#
# INPUT     :   string      - filename                    - the name of the dataset file

#
# OUTPUT    :   none
#
# ************************************************
visualiseDataset <- function(filename){
  #Add percentage labels to the churn plot
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
  
  paymentPcts <-  dataset$PaymentMethod
  paymentPcts <- list("Payment Method" = 0, "n" = 0,"%" = 0,"per_label" = 0)
  literals<-as.vector(unique(dataset$PaymentMethod))
  
  # results <- data.frame()
  # for(literal in literals){
  #   count <- sum(dataset$PaymentMethod == literal)
  #   pct <- count/nrow(dataset)
  #   pct_label <- round(pct*100,2)
  #   result <- list("Payment Method" = literal, "n" = count,"%" = pct,"per_label" = pct_label)
  #   result <- t(result)
  #   results <-rbind(results, data.frame(result))
  #   
  #   
  # }
  # 
  # print(results)
  
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

