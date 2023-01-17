# Valuation UST Test.R
# rmarkdown::render("Valuation UST Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
Packages <- c("date", "optimx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Test Inputs for U. S. Treasury bonds
#
MarketQuotedBondPrice = 975312.50 # Dollars: Quoted price without accrued interest
inputFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
inputCouponRate = 2.25      # Percent
inputPar = 1000000.0        # Currency
inputYieldToMaturity = 2.535235  # Percent
# Dollars: Quoted bond price without accrued interest (stubbed to -99)
inputBondPrice = -99       
SettlementDateMonth = 1     # Integer: 1-12
SettlementDateDay = 17      # Integer: 1-31
SettlementDateYear = 2018   # Integer: 1-very high number
MaturityDateMonth = 11      # Integer: 1-12
MaturityDateDay = 15        # Integer: 1-31
MaturityDateYear = 2027     # Integer: 1-very high number
#
# UST functions (semi-annual only)
#
source("UST Functions.R")
BONDInputData <- list(inputFrequency, inputCouponRate, inputPar, 
  inputYieldToMaturity, inputBondPrice, 
  SettlementDateMonth, SettlementDateDay, SettlementDateYear, 
  MaturityDateMonth, MaturityDateDay, MaturityDateYear)
names(BONDInputData) <- c("Frequency", "CouponRate", "Par", 
  "YieldToMaturity", "BondPrice", 
  "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
  "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear")
# Data frame easier to manage later
BONDInputData <- as.data.frame(BONDInputData)
# # Eventually build out to handle datasets
# BONDInputData2 <- BONDInputData
# BONDInputData2$YieldToMaturity <- BONDInputData2$YieldToMaturity + 1
# BONDInputData <- rbind(BONDInputData, BONDInputData2)
#
# Calendar manipulations
#
N = CouponsRemaining(BONDInputData)
# ElapsedOutput contains fraction, JLastDate, JNextDate, and JCurrentDate
ElapsedOutput = Elapsed(BONDInputData)
# Number of Total Days
NTD <- ElapsedOutput$NextDate - ElapsedOutput$LastDate
# Number of Accrued Days since last semi-annual coupon
NAD <- ElapsedOutput$Fraction * NTD
# Fraction of coupon period that has elapsed already
f <- ElapsedOutput$Fraction
# Bond maturity, in years
Mat <- TimeToMaturity(BONDInputData)
NAD; NTD; f; N; Mat
#
# Bond value given yield to maturity
#
MarketValueOfBond = BondValue(BONDInputData)
AccruedInterestAmount = AccruedInterest(BONDInputData)
ModelQuotedBondPrice = MarketValueOfBond - AccruedInterestAmount
MarketValueOfBond; AccruedInterestAmount; 
ModelQuotedBondPrice; MarketQuotedBondPrice
#
# Yield to maturity given bond value
#
inputBondPrice = MarketQuotedBondPrice #Dollars:Quoted price w/o accrued interest
BONDInputData$BondPrice <- inputBondPrice
EstYieldToMaturity = YieldToMaturitySolver(BONDInputData)
EstYieldToMaturity; inputYieldToMaturity

