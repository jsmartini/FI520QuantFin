# 3.5 LSC Regression Independent Variables Test.R
# rmarkdown::render("3.5 LSC Regression Independent Variables Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Inputs
NumberOfFactors <- 5 # Maximum is five
NScalars <- pmax(1, NumberOfFactors - 2)
Scalars <- c(1:NScalars)
Scalars[1] <- 2.0
if(NScalars > 1)Scalars[2] <- 5.0
if(NScalars > 2)Scalars[3] <- 8.0
if(NScalars > 3)Scalars[4] <- 11.0
NumberOfMaturities <- 31
NumberOfFactors <- 1
source("LSC Regression Independent Variables Graph.R")
NumberOfFactors <- 2
source("LSC Regression Independent Variables Graph.R")
NumberOfFactors <- 3
source("LSC Regression Independent Variables Graph.R")
NumberOfFactors <- 4
source("LSC Regression Independent Variables Graph.R")
NumberOfFactors <- 5
source("LSC Regression Independent Variables Graph.R")
# Role of scalar
NumberOfFactors <- 3
Scalars[1] <- 0.5
source("LSC Regression Independent Variables Graph.R")
Scalars[1] <- 2.0
source("LSC Regression Independent Variables Graph.R")
Scalars[1] <- 5.0
source("LSC Regression Independent Variables Graph.R")
Scalars[1] <- 20.0
source("LSC Regression Independent Variables Graph.R")


