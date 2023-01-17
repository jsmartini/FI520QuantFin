# GBM CO Test.R
# rmarkdown::render("GBM CO Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries: pracma: integral2
Packages <- c("pracma", "mvtnorm", "pbivnorm") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Input list
#
inputiC <- 1                              # 1-CO call, -1-CO put
inputiU <- 1                              # 1-UO call, -1-UO put
inputUnderlying <- 100.0                  # Must be positive
inputUnderlyingStrikePrice <- 100.0       # Must be positive
inputCompoundStrikePrice <- 10            # Must be positive
inputInterestRate <- 2.0                  # Must be positive
inputUnderlyingYield <- 0.0               # Payout of underlying instrument
inputOptionYield <- 5.0                   # Payout of option
inputVolatility <- 30.0                   # Must be positive
inputUnderlyingTimeToMaturity <- 5.0      # Must be positive
inputCompoundTimeToMaturity <- 1.0        # Must be positive
COInputData <- list(inputiC, inputiU, inputUnderlying, 
  inputUnderlyingStrikePrice, inputCompoundStrikePrice, inputInterestRate,
  inputUnderlyingYield, inputOptionYield, inputVolatility, 
  inputUnderlyingTimeToMaturity, inputCompoundTimeToMaturity) 
names(COInputData) <- c("iC", "iU", "S", "XU", "XC", "r", "d", "q", "v", 
  "TU", "TC")
source("GBM COVM Functions.R")
# Boundaries for critical stock price iterative search
LowerBound <- 0.0
UpperBound <- 5.0*COInputData$S #Critical stock price for put has 2 solutions
# Underlying GBMOVM test
inputUnderlying <- COInputData$S
inputUnderlyingStrikePrice <- COInputData$XU
inputInterestRate <- COInputData$r
inputUnderlyingYield <- COInputData$d
inputOptionYield <- COInputData$q
inputVolatility <- COInputData$v
inputTimeToMaturity <- COInputData$TU
inputType <- COInputData$iU
GBMInputData <- list(inputUnderlying, inputUnderlyingStrikePrice, 
  inputInterestRate, inputUnderlyingYield,  inputOptionYield, 
  inputVolatility, inputTimeToMaturity, inputType)
names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
  "DividendYield", "OptionYield", "Volatility", "TimeToMaturity", "Type")
UCallValue <<- GBMOptionValue(GBMInputData)
GBMInputData$Type <- -1
UPutValue <<- GBMOptionValue(GBMInputData)
UCallValue; UPutValue
#
# Compound option base case
#
COInputData$iC <- 1
COInputData$iU <- 1
CoCCSP <- COCriticalStockPrice(COInputData, UCallValue, LowerBound, UpperBound)
CoC <- COValue(COInputData, LowerBound, UpperBound)
COInputData$iU <- -1
CoPCSP <- COCriticalStockPrice(COInputData, UPutValue, LowerBound, UpperBound)
CoP <- COValue(COInputData, LowerBound, UpperBound)
COInputData$iC <- -1
COInputData$iU <- 1
PoCCSP <- COCriticalStockPrice(COInputData, UCallValue, LowerBound, UpperBound)
PoC <- COValue(COInputData, LowerBound, UpperBound)
COInputData$iU <- -1
PoPCSP <- COCriticalStockPrice(COInputData, UPutValue, LowerBound, UpperBound)
PoP <- COValue(COInputData, LowerBound, UpperBound)
CoCCSP; CoPCSP; PoCCSP; PoPCSP
CoC; CoP; PoC; PoP
# Compound options lower bounds
COInputData$iC <- 1
COInputData$iU <- 1
CoCLB <- COLowerBound(COInputData)
COInputData$iC <- 1
COInputData$iU <- -1
CoPLB <- COLowerBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- 1
PoCLB <- COLowerBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- -1
PoPLB <- COLowerBound(COInputData)
CoCLB; CoPLB; PoCLB; PoPLB
# Compound options upper bounds
COInputData$iC <- 1
COInputData$iU <- 1
CoCUB <- COUpperBound(COInputData)
COInputData$iC <- 1
COInputData$iU <- -1
CoPUB <- COUpperBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- 1
PoCUB <- COUpperBound(COInputData)
COInputData$iC <- -1
COInputData$iU <- -1
PoPUB <- COUpperBound(COInputData)
CoCUB; CoPUB; PoCUB; PoPUB
# Plot wrt Underlying Instrument
NumberOfObservations <- 101
Increment <- 0.75
SLowerBound <- COInputData$S*(1 - Increment)
SUpperBound <- COInputData$S*(1 + Increment)
source("GBM CO Sensitivity Analysis wrt Stock Price.R")
