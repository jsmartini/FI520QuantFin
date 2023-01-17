# 5.1 Valuation Option Boundaries Pre During Post Pandemic Test.R
# rmarkdown::render("5.1 Valuation Option Boundaries Test.R", 
#  "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
#par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
Packages <- c("data.table", "date", "stats", "tis", "gtools", "openxlsx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Plots to produce
CallLB <- TRUE
CallTV <- FALSE
PutLB <- TRUE
PutTV <- FALSE
# Fix call axis for lower bounds and time values
FixXRangeCLB <- TRUE
FixYRangeCLB <- TRUE
MinXRangeCLB <- 75
MaxXRangeCLB <- 125
MinYRangeCLB <- 0
MaxYRangeCLB <- 30
FixXRangeCTV <- TRUE
FixYRangeCTV <- TRUE
MinXRangeCTV <- 75
MaxXRangeCTV <- 125
MinYRangeCTV <- 0
MaxYRangeCTV <- 15
# Fix put axis for lower bounds and time values
FixXRangePLB <- TRUE
FixYRangePLB <- TRUE
MinXRangePLB <- 75
MaxXRangePLB <- 125
MinYRangePLB <- 0
MaxYRangePLB <- 30
FixXRangePTV <- TRUE
FixYRangePTV <- TRUE
MinXRangePTV <- 75
MaxXRangePTV <- 125
MinYRangePTV <- 0
MaxYRangePTV <- 15
# Put-call parity range
FixXRangePCP <- TRUE
FixYRangePCP <- TRUE
MinXRangePCP <- 75
MaxXRangePCP <- 125
MinYRangePCP <- 0
MaxYRangePCP <- 30
#
# Analysis of half year options
#
# Analysis of call lower bound
FileName = "SPYHalf20051215.xlsx"
source('Call Lower Bound.R')
FileName = "SPYHalf20081215.xlsx"
source('Call Lower Bound.R')
FileName = "SPYHalf20111215.xlsx"
source('Call Lower Bound.R')
# Analysis of put lower bound
FileName = "SPYHalf20051215.xlsx"
source('Put Lower Bound.R')
FileName = "SPYHalf20081215.xlsx"
source('Put Lower Bound.R')
FileName = "SPYHalf20111215.xlsx"
source('Put Lower Bound.R')
# Analysis of put call parity bounds
FileName = "SPYHalf20051215.xlsx"
source('Put Call Parity.R')
FileName = "SPYHalf20081215.xlsx"
source('Put Call Parity.R')
FileName = "SPYHalf20111215.xlsx"
source('Put Call Parity.R')
#
# Analysis of quarter year options
#
# Analysis of call lower bound
FileName = "SPYQuarter20051215.xlsx"
source('Call Lower Bound.R')
FileName = "SPYQuarter20081215.xlsx"
source('Call Lower Bound.R')
FileName = "SPYQuarter20111215.xlsx"
source('Call Lower Bound.R')
# Analysis of put lower bound
FileName = "SPYQuarter20051215.xlsx"
source('Put Lower Bound.R')
FileName = "SPYQuarter20081215.xlsx"
source('Put Lower Bound.R')
FileName = "SPYQuarter20111215.xlsx"
source('Put Lower Bound.R')
# Analysis of put call parity bounds
FileName = "SPYQuarter20051215.xlsx"
source('Put Call Parity.R')
FileName = "SPYQuarter20081215.xlsx"
source('Put Call Parity.R')
FileName = "SPYQuarter20111215.xlsx"
source('Put Call Parity.R')
#
# Analysis of monthly options
#
# Analysis of call lower bound
FileName = "SPYMonth20051215.xlsx"
source('Call Lower Bound.R')
FileName = "SPYMonth20081215.xlsx"
source('Call Lower Bound.R')
FileName = "SPYMonth20111215.xlsx"
source('Call Lower Bound.R')
# Analysis of put lower bound
FileName = "SPYMonth20051215.xlsx"
source('Put Lower Bound.R')
FileName = "SPYMonth20081215.xlsx"
source('Put Lower Bound.R')
FileName = "SPYMonth20111215.xlsx"
source('Put Lower Bound.R')
# Analysis of put call parity bounds
FileName = "SPYMonth20051215.xlsx"
source('Put Call Parity.R')
FileName = "SPYMonth20081215.xlsx"
source('Put Call Parity.R')
FileName = "SPYMonth20111215.xlsx"
source('Put Call Parity.R')
