# Valuation GBM Binomial OVM European-Style Test.R
# Geometric Brownian Motion
# Illustrating European-style binomial option valuation and related functions in R
# rmarkdown::render("Valuation GBM Binomial OVM European-Style Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
PackagesToLibrary <- c("beepr") # Libraries
if (length(setdiff(PackagesToLibrary, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(PackagesToLibrary, rownames(installed.packages())))  
} # Make sure libraries are installed on this computer
lapply(PackagesToLibrary,library,character.only = TRUE) # Load and attach libraries
rm(PackagesToLibrary)
source('ESGBMBINOVM Functions.R')
# Test inputs
inputStockPrice = 100.0          # Need "input" as using variable names below
inputStrikePrice = 100.0         # In currency units, numeric
inputInterestRate = 5.0          # In percent
inputDividendYield = 0.0         # In percent
inputVolatility = 30.0           # In percent
inputTimeToMaturity = 1.0        # In fraction of year
inputType = 1L                   # 1 for call, -1 for put
inputNumberOfSteps = as.integer(500)   # Or use L: 1000L
inputPayoutType = 1L             # 1 Plain vanilla, 2 digital
inputEMMProbability = 50.0       # In percent
inputDigitalPayout = 100.0
LowerBoundSP = 50 # Analysis wrt stock price
UpperBoundSP = 150
NumberOfObservations = 101
StepSize = 1L # Analysis wrt number of steps
MinStep = 5L # Must be multiple of StepSize
MaxStep = 500L # Must be multiple of StepSize
# Plot footers
TS = paste0('S=', inputStockPrice)
TX = paste0(',X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
Td = paste0(',d=', inputDividendYield, ',')
TV = paste0('Vol=', inputVolatility)
TT = paste0(',T=',inputTimeToMaturity)
TN = paste0(',N=',inputNumberOfSteps)
TDP = paste0(',DP=', inputDigitalPayout)
sTitleBIN = paste0(TS,TX, TR, Td, TV, TT, TN)
sTitleBINDP = paste0(TS,TX, TR, Td, TV, TT, TN, TDP)
#  BINInputData - list of inputs with associated names
BINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType, 
  inputNumberOfSteps, inputPayoutType, inputEMMProbability, 
  inputDigitalPayout)
names(BINInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type", "NumberOfSteps", 
  "PayoutType", "EMMProbability", "DigitalPayout")
is.integer(BINInputData$Type)
BINInputData
BINInputData$Type = 1L
BINInputData$PayoutType = 1L
PVCallLB = ESBINOptionLowerBound(BINInputData) 
PVCallLB
PVCallUB = ESBINOptionUpperBound(BINInputData) 
PVCallUB
PVCallValue = ESBINOptionValue(BINInputData) 
PVCallValue
BINInputData$Type = -1L
PVPutLB = ESBINOptionLowerBound(BINInputData) 
PVPutLB
PVPutUB = ESBINOptionUpperBound(BINInputData) 
PVPutUB
PVPutValue = ESBINOptionValue(BINInputData) 
PVPutValue
BINInputData$PayoutType = 2L
BINInputData$Type = 1L
DigitalCallValue = ESBINOptionValue(BINInputData) 
DigitalCallValue
BINInputData$PayoutType = 2L
BINInputData$Type = -1L
DigitalPutValue = ESBINOptionValue(BINInputData) 
DigitalPutValue
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
PV
DV = DigitalCallValue + DigitalPutValue
DV
Diff = PV - DV
Diff
# StockPrice
StepSizeSP = (UpperBoundSP - LowerBoundSP)/(NumberOfObservations - 1)
StockPrice <- c(1:NumberOfObservations)
PVCallLB <- c(1:NumberOfObservations)
PVCallUB <- c(1:NumberOfObservations)
PVCallValue <- c(1:NumberOfObservations)
PVPutLB <- c(1:NumberOfObservations)
PVPutUB <- c(1:NumberOfObservations)
PVPutValue <- c(1:NumberOfObservations)
DigitalCallValue <- c(1:NumberOfObservations)
DigitalPutValue <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-LowerBoundSP + (i - 1)*StepSizeSP
  BINInputData$StockPrice = StockPrice[i]
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  PVCallLB[i] = ESBINOptionLowerBound(BINInputData)
  PVCallUB[i] = ESBINOptionUpperBound(BINInputData)
  PVCallValue[i] = ESBINOptionValue(BINInputData)
  BINInputData$Type = -1L
  PVPutLB[i] = ESBINOptionLowerBound(BINInputData)
  PVPutUB[i] = ESBINOptionUpperBound(BINInputData)
  PVPutValue[i] = ESBINOptionValue(BINInputData)
  BINInputData$PayoutType = 2L
  BINInputData$Type = 1L
  DigitalCallValue[i] = ESBINOptionValue(BINInputData)
  BINInputData$PayoutType = 2L
  BINInputData$Type = -1L
  DigitalPutValue[i] = ESBINOptionValue(BINInputData)
}
BINInputData$StockPrice = inputStockPrice # Reset from previous analysis
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#  Call with boundaries
MaxValue = max(PVCallValue, PVCallLB, PVCallUB)
MinValue = min(PVCallValue, PVCallLB, PVCallUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
mTitle = "European-Style Binomial Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(StockPrice, PVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PVCallLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PVCallUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#  Call time value
PVCallTV <- PVCallValue - PVCallLB
MaxValue = max(PVCallTV)
MinValue = min(PVCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "European-Style Binomial Option Time Value (GBM)"
xTitle = "Stock Price"
yTitle = "Call Time Value"
plot(StockPrice, PVCallTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Put with boundaries
MaxValue = max(PVPutValue, PVPutLB, PVPutUB)
MinValue = min(PVPutValue, PVPutLB, PVPutUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
mTitle = "European-Style Binomial Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(StockPrice, PVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PVPutLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PVPutUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#  Put time value
PVPutTV <- PVPutValue - PVPutLB
MaxValue = max(PVPutTV)
MinValue = min(PVPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "European-Style Binomial Option Time Value (GBM)"
xTitle = "Stock Price"
yTitle = "Put Time Value"
plot(StockPrice, PVPutTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Calls and puts
MaxValue = max(PVCallValue, PVPutValue)
MinValue = min(PVCallValue, PVPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, PVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PVPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
# Digital Options with respect to the Stock Price
MaxValue = max(DigitalCallValue, DigitalPutValue)
MinValue = min(DigitalCallValue, DigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Digital Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(StockPrice, DigitalCallValue, type = "b", main = mTitle,
  sub = sTitleBINDP, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, DigitalPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
#
# Plots with Number Of Steps
#
StepCount = as.integer((as.numeric(MaxStep)-as.numeric(MinStep)) /
  as.numeric(StepSize) + 1)
NumberOfSteps = as.integer(seq(MinStep,MaxStep,StepSize))
PVCallValue <- c(1:StepCount)
PVPutValue <- c(1:StepCount)
DigitalCallValue <- c(1:StepCount)
DigitalPutValue <- c(1:StepCount)
j=0L
for(i in seq(MinStep, MaxStep, StepSize)){
  j = j + 1
  BINInputData$NumberOfSteps = as.integer(i)
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  # PVCallValue[j] = i
  PVCallValue[j] = ESBINOptionValue(BINInputData)
  BINInputData$Type = -1L
  PVPutValue[j] = ESBINOptionValue(BINInputData)
  BINInputData$PayoutType = 2L
  BINInputData$Type = 1L
  DigitalCallValue[j] = ESBINOptionValue(BINInputData)
  BINInputData$PayoutType = 2L
  BINInputData$Type = -1L
  DigitalPutValue[j] = ESBINOptionValue(BINInputData)
}
BINInputData$NumberOfSteps = inputNumberOfSteps
# Plain vanilla option values with respect to the number of steps
xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
MaxOptionValue = max(PVCallValue,PVPutValue)
MinOptionValue = min(PVCallValue,PVPutValue)
ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Plain Vanilla Option Value (GBM)"
xTitle = "Number of Steps"
yTitle = "Plain Vanilla Option Value"
lTitle = "Parameter"
plot(NumberOfSteps, PVCallValue, type="b", main=mTitle, sub=sTitleBIN,
  xlab="Number Of Steps", ylab="Option Values", col="black",
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(NumberOfSteps, PVPutValue, type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("right", legtxt, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# Digital option values with respect to the number of steps
MaxOptionValue = max(DigitalCallValue, DigitalPutValue)
MinOptionValue = min(DigitalCallValue, DigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Digital Option Value (GBM)"
xTitle = "Stock Price"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(NumberOfSteps, DigitalCallValue, type="b",
  main=mTitle, sub=sTitleBINDP, pch = 1, cex = 0.5,
  xlab="Number Of Steps", ylab="Digital Option Values",
  col="black", xlim = xlim1, ylim = ylim1)
lines(NumberOfSteps, DigitalPutValue, col="black",type="b", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program

