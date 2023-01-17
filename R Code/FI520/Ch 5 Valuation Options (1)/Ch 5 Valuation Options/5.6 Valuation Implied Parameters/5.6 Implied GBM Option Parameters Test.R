# 5.6 Implied GBM Option Parameters Test.R
# rmarkdown::render("5.6 Implied GBM Option Parameters Test.R", 
#  "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Available functions
#  GBMInputData - list of inputs with associated names
#  PV1(Maturity, Rate) - present value of $1
#  B = GBMInputData
#  d1(B) - value of d1
#  d2(B) - value of d2
#  n(d) - standard normal PDF, given scalar d
#  N(d) - standard normal CDF, given scalar d
#  GBMOptionValue(B) - option value, type = 1 is call, type = 2 is put
#  OptionLowerBound(B) - option lower bounds
#  GBMDYOptionImpliedVolatility(B, inputOptionValue) - implied volatility
#  GBMDYOptionImpliedStockPrice(B, inputOptionValue) - implied stock price
#  GBMDYOptionImpliedStrikePrice(B, inputOptionValue) - implied strike price
#  GBMDYOptionImpliedTimeToMaturity(B, inputOptionValue)-implied maturity
#  GBMDYOptionImpliedInterestRate(B, inputOptionValue) - implied interest rate
#  GBMDYOptionImpliedDividendYield(B, inputOptionValue)-implied dividend yield
#
# Test Data
inputStockPrice = 100          # Need "input" as using variable names below
inputStrikePrice = 100
inputInterestRate = 5.0        # In percent
inputDividendYield = 0.0       # In percent
inputVolatility = 30.0   # In percent
inputTimeToMaturity = 1
inputType = 1 # 1 for call, -1 for put
inputOptionValue = 14.23125
inputImpliedLowerBound <- 0.0
inputImpliedUpperBound <- 1000
#  Input matrix 
#  GBMInputData - list of inputs with associated names
GBMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType,
  inputImpliedLowerBound, inputImpliedUpperBound)
names(GBMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type",
  "ImpliedLowerBound", "ImpliedUpperBound")
source("GBMOVM and Extended Functions.R")
# Test implied volatility
GBMInputData$Volatility <- -99
ImpliedCallVolatility = GBMDYOptionImpliedVolatility(GBMInputData, 
  inputOptionValue)
GBMInputData$Volatility <- inputVolatility
CallValue = GBMOptionValue(GBMInputData)
inputVolatility; ImpliedCallVolatility; CallValue
#
# Test functions
#
UIValue = GBMInputData$StockPrice # Underlying instrument value
TestPV1 = PV1(inputTimeToMaturity, inputInterestRate)
Testd1 = d1(GBMInputData)
Testd2 = d2(GBMInputData)
Testn = n(d1(GBMInputData))
TestN = N(d1(GBMInputData))
UIValue; TestPV1; Testd1; Testd2; Testn; TestN

GBMInputData$Type = 1  # Call
CallLowerBound = OptionLowerBound(GBMInputData)
CallValue = GBMOptionValue(GBMInputData)
GBMInputData$Type = -1  # Put
PutLowerBound = OptionLowerBound(GBMInputData)
PutValue = GBMOptionValue(GBMInputData)
GBMInputData$Type = inputType   # Reset to original value
CallLowerBound; CallValue; PutLowerBound; PutValue
# CallValue from GBMOptionValue function computed above
inputOptionValue = CallValue  
ImpliedCallVolatility = GBMDYOptionImpliedVolatility(GBMInputData, 
  inputOptionValue)
ImpliedCallStockPrice = GBMDYOptionImpliedStockPrice(GBMInputData, 
  inputOptionValue)
# Test data for strike price
GBMInputData$StockPrice = 100          
GBMInputData$StrikePrice = -99
GBMInputData$InterestRate = 5.0        
GBMInputData$DividendYield = 0.0       
GBMInputData$Volatility = 30.0   
GBMInputData$TimeToMaturity = 1
GBMInputData$Type = 1 
inputNewOptionValue = 8.0
GBMInputData$ImpliedLowerBound <- 0.0
GBMInputData$ImpliedUpperBound <- 1000
ImpliedCallStrikePrice = GBMDYOptionImpliedStrikePrice(GBMInputData, 
  inputNewOptionValue)
GBMInputData$StrikePrice <- ImpliedCallStrikePrice
NewCallValue = GBMOptionValue(GBMInputData)
ImpliedCallStrikePrice; NewCallValue
GBMInputData$StrikePrice <- inputStrikePrice
ImpliedCallTimeToMaturity = GBMDYOptionImpliedTimeToMaturity(GBMInputData, 
  inputOptionValue)
# Interest rates and dividend yields can be zero or negative, sensitive to
#  extreme boundaries
# Test data for interest rates
GBMInputData$StockPrice = 100          
GBMInputData$StrikePrice = 100
GBMInputData$InterestRate = -99.0        
GBMInputData$DividendYield = 0.0       
GBMInputData$Volatility = 30.0   
GBMInputData$TimeToMaturity = 1
GBMInputData$Type = 1 
GBMInputData$ImpliedLowerBound <- -10
GBMInputData$ImpliedUpperBound <- 10
inputNewOptionValue <- inputOptionValue*1.01
ImpliedCallInterestRate = GBMDYOptionImpliedInterestRate(GBMInputData, 
  inputNewOptionValue)
GBMInputData$InterestRate <- ImpliedCallInterestRate
NewCallValue = GBMOptionValue(GBMInputData)
PCCallValue = 100 * (NewCallValue - inputOptionValue)/inputOptionValue
PCInterestRate = 100 * (ImpliedCallInterestRate - inputInterestRate) / 
  inputInterestRate
ImpliedCallInterestRate; PCInterestRate; PCCallValue
# Dividend yield
GBMInputData$ImpliedLowerBound = -10
GBMInputData$ImpliedUpperBound = 10
ImpliedCallInterestRate = GBMDYOptionImpliedInterestRate(GBMInputData, 
  inputOptionValue)
ImpliedCallDividendYield = GBMDYOptionImpliedDividendYield(GBMInputData, 
  inputOptionValue)
GBMInputData$ImpliedLowerBound = inputImpliedLowerBound
GBMInputData$ImpliedUpperBound = inputImpliedUpperBound
CallValue; ImpliedCallVolatility; ImpliedCallStockPrice;ImpliedCallStrikePrice
ImpliedCallTimeToMaturity; ImpliedCallInterestRate; ImpliedCallDividendYield  

GBMInputData$Type = -1        # Put
# PutValue from GBMOptionValue function computed above
inputOptionValue = PutValue  
ImpliedPutVolatility = GBMDYOptionImpliedVolatility(GBMInputData, 
  inputOptionValue)
ImpliedPutStockPrice = GBMDYOptionImpliedStockPrice(GBMInputData, 
  inputOptionValue)
ImpliedPutStrikePrice = GBMDYOptionImpliedStrikePrice(GBMInputData, 
  inputOptionValue)
ImpliedPutTimeToMaturity = GBMDYOptionImpliedTimeToMaturity(GBMInputData, 
  inputOptionValue)
ImpliedPutInterestRate = GBMDYOptionImpliedInterestRate(GBMInputData, 
  inputOptionValue)
ImpliedPutDividendYield = GBMDYOptionImpliedDividendYield(GBMInputData, 
  inputOptionValue)
PutValue; ImpliedPutVolatility; ImpliedPutStockPrice; ImpliedPutStrikePrice
# Note: Put time to maturity has multiple solutions and rate range too large
ImpliedPutTimeToMaturity; ImpliedPutInterestRate; ImpliedPutDividendYield  
# Wide boundary range
GBMInputData$ImpliedLowerBound <- 0.0
GBMInputData$ImpliedUpperBound <- 1000
GBMInputData$Type = -1        # Put
PutValue = GBMInputData$OptionValue = GBMOptionValue(GBMInputData)  
GBMInputData$TimeToMaturity = -99
ImpliedPutTimeToMaturity = GBMDYOptionImpliedTimeToMaturity(GBMInputData, 
  inputOptionValue)
ImpliedPutTimeToMaturity; PutValue
# Narrower boundary range
GBMInputData$ImpliedLowerBound <- 0.0
GBMInputData$ImpliedUpperBound <- 10
GBMInputData$Type = -1        # Put
GBMInputData$TimeToMaturity = inputTimeToMaturity
PutValue = GBMInputData$OptionValue = GBMOptionValue(GBMInputData)  
GBMInputData$TimeToMaturity = -99
ImpliedPutTimeToMaturity = GBMDYOptionImpliedTimeToMaturity(GBMInputData, 
  inputOptionValue)
ImpliedPutTimeToMaturity; PutValue
# Retry with narrower range
# Interest rates and dividend yields can be zero or negative, sensitive to
#  extreme boundaries
GBMInputData$ImpliedLowerBound = 1
GBMInputData$ImpliedUpperBound = 10
GBMInputData$TimeToMaturity = inputTimeToMaturity
ImpliedPutInterestRate = GBMDYOptionImpliedInterestRate(GBMInputData, 
  inputOptionValue)
ImpliedPutTimeToMaturity = GBMDYOptionImpliedTimeToMaturity(GBMInputData, 
  inputOptionValue)
GBMInputData$ImpliedLowerBound = inputImpliedLowerBound
GBMInputData$ImpliedUpperBound = inputImpliedUpperBound
ImpliedPutTimeToMaturity; ImpliedPutInterestRate
# Example of solving for implied firm value
GBMInputData$StockPrice <- -99    # Firm value
GBMInputData$StrikePrice <- 137.0 # Debt par value
GBMInputData$InterestRate <- 5.0  # Risk free rate
GBMInputData$DividendYield <- 0.0
GBMInputData$TimeToMaturity <- 5.0  # Years to debt maturity
GBMInputData$Volatility <- 27.125   # Volatility of firm
GBMInputData$Type <- 1 # Call option
inputEquityValue = 100.0  # Equity value
ImpliedFirmValue = GBMDYOptionImpliedStockPrice(GBMInputData, inputEquityValue)
ImpliedFirmValue
# Example of solving for implied firm value
GBMInputData$Volatility <- 27.125 + 1.0   # Volatility of firm
GBMInputData$Type <- 1 # Call option
inputEquityValue = 100.0  # Equity value
ImpliedFirmValue2 = GBMDYOptionImpliedStockPrice(GBMInputData, inputEquityValue)
ImpliedFirmValue2
#
# Plot: Firm value and firm volatility
#
TS = paste0('FV=', round(ImpliedFirmValue,0))
TX = paste0(',D(T)=', GBMInputData$StrikePrice)
TR = paste0(',r=', GBMInputData$InterestRate)
TV = paste0(',FVol=', GBMInputData$Volatility)
TT = paste0(',T=', GBMInputData$TimeToMaturity)
sTitle = paste0(TS, TX, TR, TV, TT)
inputEquityValue = 100.0  # Equity value
NumberOfObservations = 100
LowerBound = 0.75*GBMInputData$Volatility # Note centering on original values
UpperBound = 1.25*GBMInputData$Volatility
StepSize = (UpperBound - LowerBound)/NumberOfObservations
FirmVolatility <- c(1:NumberOfObservations)
FirmValue <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  FirmVolatility[i] <- as.double(LowerBound + (i-1)*StepSize)
  GBMInputData$Volatility = FirmVolatility[i]
  FirmValue[i] = GBMDYOptionImpliedStockPrice(GBMInputData, inputEquityValue)
}
# Option value plots
y = FirmValue
x = FirmVolatility
MaxValue = max(y); MinValue = min(y)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(x); MinValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
mTitle = "Firm Value With Respect To Firm Volatility"
xTitle = "Firm Volatility"
yTitle = "Firm Value"
plot(x, y, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Comments: As volatility increases, we expect equity (call on firm)
#  to increase. We fix, however, equity value and allow volatility to
#  change. To keep equity value constant, firm value must decrease.

