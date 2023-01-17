# 4.3 NDDM with LSC Fit Test.R
# rmarkdown::render("4.3 NDDM with LSC Fit Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  stats: integrate function
Packages <- c("stats", "openxlsx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Plots to produce:
PlotPVD <- TRUE
PlotlnPVD <- FALSE # Needs different transformation than LSC because linear
# Number of factors to fit:
LSC1 <- TRUE
LSC2 <- TRUE
LSC3 <- TRUE
LSC4 <- TRUE
LSC5 <- TRUE
LSC6 <- TRUE
# Fit forward version of LSC
ForwardLSC <- FALSE # Not covered in this module
# NDDM Inuts from file created by 4.3 NDDM with LSC Fit Test.R
Div <- read.xlsx(xlsxFile = "Dividends.xlsx", sheet = 1, skipEmptyRows=FALSE)
Series <- read.xlsx(xlsxFile = "Series.xlsx", sheet = 1, skipEmptyRows=FALSE)
LengthDiv <- length(Div$PVD)
LengthSeries <- length(Series$SeriesValue)
StockValue <- Series$SeriesValue[LengthSeries]
SumDiv <- 0
SumWeight <- 0
for(i in 1:LengthDiv){
  Div$lnPVD[i] <- log(Div$PVD[i])
  Div$Weight[i] = Div$PVD[i] / StockValue
  SumDiv = SumDiv + Div$PVD[i]
  SumWeight = SumWeight + Div$Weight[i]
}
SumDiv; StockValue; SumWeight
# LSC inputs
NumberOfFactors <- 6
N <- NumberOfFactors - 2
Scalar <- c(1:N)
Scalar[1] <- 10
Scalar[2] <- 20
Scalar[3] <- 50
Scalar[4] <- 80
#
# Work on LSC fitting
#
NumberOfMaturities <- LengthDiv
Tau <- c(1:N)
Tau <- Scalar
NumberOfDates <- 1
Factors <- matrix(nrow = NumberOfFactors - 1, ncol = NumberOfMaturities)
Factors2 <- matrix(nrow = NumberOfFactors - 1, ncol = NumberOfMaturities)
NP = NumberOfFactors - 1
#
# Factors: Standard LSC factors based originally on spot rates
# Factors2: Alternative LSC factors based originally on forward rates
#  Does not appear to make a difference
#
for (j in 1:NP) {
  for (i in 1:NumberOfMaturities) {
    if (j == 1) {
      Factors[j,i] = (1.0 - exp(-Div$MaturityTime[i]/Tau[j])) / 
        (Div$MaturityTime[i]/Tau[j])
      Factors2[j,i] = exp(-Div$MaturityTime[i]/Tau[j])
    } else {
      Factors[j, i] = ((1.0 - exp(-Div$MaturityTime[i]/Tau[j-1])) /
        (Div$MaturityTime[i]/Tau[j-1])) - exp(-Div$MaturityTime[i]/Tau[j-1])
      Factors2[j, i] = (Div$MaturityTime[i]/Tau[j-1]) * 
          exp(-Div$MaturityTime[i]/Tau[j-1])
    }
  }
}
if(LSC1){
  source("One Factor Models.R")
  if(ForwardLSC) source("One Factor Models V2.R")
}
if(LSC2){
  source("Two Factor Models.R")
  if(ForwardLSC) source("Two Factor Models V2.R")
}
if(LSC3){
  source("Three Factor Models.R")
  if(ForwardLSC) source("Three Factor Models V2.R")
}
if(LSC4){
  source("Four Factor Models.R")
  if(ForwardLSC) source("Four Factor Models V2.R")
}
if(LSC5){
  source("Five Factor Models.R")
  if(ForwardLSC) source("Five Factor Models V2.R")
}
if(LSC6){
  source("Six Factor Models.R")
  if(ForwardLSC) source("Six Factor Models V2.R")
}

