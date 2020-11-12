# *************************************************************
#   PRACTICAL BUSINESS ANALYTICS
#   MARS GROUP 
#
#   RANDOM FOREST FUNCTIONS
#
#   
#   DATE:     1 NOVEMBER 2020
#   VERSION:  V1.0
#   AUTHOR:   MARS Team
#
#   UPDATE
#   1.00      11/11/2020    Chris Jennings    Initial Version
#   1.10      12/11/2020     

#  clears all objects in "global environment"
rm(list=ls())

# ************************************************
#GLOBALS

KFOLDS           <- 5

# Define and then load the libraries used in this project
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.53
# formattable 	         0.2.0.1
# stats                  4.0.3
# PerformanceAnalytics   2.0.4

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "stringr",
               "PerformanceAnalytics")


######## Main Function ########

# ************************************************
# main() :
# Main entry point
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# ************************************************
main<-function(){
  print("Cat")
  
  mars_GetPreprocessedDataset()

} #endof main()

# ************************************************
# This is where R starts execution

# Automatically release memory
gc()

# Clear plots and other graphics in RStudio output
if(!is.null(dev.list())) dev.off()
graphics.off()

# Clear all warning messages
assign("last.warning", NULL, envir = baseenv())

# Clears the RStudio console area
cat("\014")

# If library not already on your computer this will download and
# install the library. Each library is activated.
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


# Reset the pseudo-random number generator to start at the same point
set.seed(123)

print("PBA TEAM MARS: Run Forest Run")

# ************************************************
# Call the main function
main()

print("end")
