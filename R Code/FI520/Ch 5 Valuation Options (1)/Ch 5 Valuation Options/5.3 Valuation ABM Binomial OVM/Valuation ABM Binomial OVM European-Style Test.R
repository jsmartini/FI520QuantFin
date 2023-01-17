# Valuation ABM Binomial OVM European Test.R
# Arithmetic Brownian Motion
# Illustrating European-style binomial option valuation and related functions
# rmarkdown::render("3.2a European-Style Binomial ABM Test.R", "word_document")
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
source('ESABMBINOVM Backward Recursion Function.R')
# Test inputs
inputStockPrice = 100.0          # Need "input" as using variable names below
inputStrikePrice = 100.0         # In currency units, numeric
inputInterestRate = 5.0          # In percent
inputDividendYield = 0.0         # In percent
inputVolatility = 29.88476829    # 29.88476829   # In dollars, annualized
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
TV = paste0('Vol=', round(inputVolatility,4))
TT = paste0(',T=',inputTimeToMaturity)
TN = paste0(',N=',inputNumberOfSteps)
TDP = paste0(',DP=', inputDigitalPayout)
sTitleBIN = paste0(TS, TX, TR, Td, TV, TT, TN)
sTitleBINDP = paste0(TS, TX, TR, Td, TV, TT, TN, TDP)
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
BINInputData$Type <- 1
BINInputData$PayoutType <- 1
ESPVCallLB = ESOptionLowerBound(BINInputData)
ESPVCallLB
ESPVCallUB = ESOptionUpperBound(BINInputData)
ESPVCallUB
BINInputData$Type <- -1
ESPVPutLB = ESOptionLowerBound(BINInputData)
ESPVPutLB
ESPVPutUB = ESOptionUpperBound(BINInputData)
ESPVPutUB
# Note: Due to backward recursion, we compute both put and call values
#  Plain vanilla (PV) as well as digital
Values = ABMESOptionValue(BINInputData)
ESPVCallValue <- Values$CallValue
ESPVPutValue <- Values$PutValue
DigitalCallValue <- Values$DigitalCallValue
DigitalPutValue <- Values$DigitalPutValue
ESPVCallValue; ESPVPutValue; DigitalCallValue; DigitalPutValue
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
DV = DigitalCallValue + DigitalPutValue
Diff = PV - DV
PV; DV; Diff
# StockPrice
StepSizeSP = (UpperBoundSP - LowerBoundSP)/(NumberOfObservations - 1)
StockPrice <- numeric(NumberOfObservations)
ESPVCallLB <- numeric(NumberOfObservations)
ESPVCallUB <- numeric(NumberOfObservations)
ESPVCallValue <- numeric(NumberOfObservations)
ESPVPutLB <- numeric(NumberOfObservations)
ESPVPutUB <- numeric(NumberOfObservations)
ESPVPutValue <- numeric(NumberOfObservations)
DigitalCallValue <- numeric(NumberOfObservations)
DigitalPutValue <- numeric(NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-LowerBoundSP + (i - 1)*StepSizeSP
  BINInputData$StockPrice = StockPrice[i]
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ESPVCallLB[i] = ESOptionLowerBound(BINInputData)
  ESPVCallUB[i] = ESOptionUpperBound(BINInputData)
  BINInputData$Type = -1L
  ESPVPutLB[i] = ESOptionLowerBound(BINInputData)
  ESPVPutUB[i] = ESOptionUpperBound(BINInputData)
  Value <- ABMESOptionValue(BINInputData)
  ESPVCallValue[i] = Value$CallValue
  ESPVPutValue[i] = Value$PutValue
  DigitalCallValue[i] = Value$DigitalCallValue
  DigitalPutValue[i] = Value$DigitalPutValue
}
BINInputData$StockPrice = inputStockPrice # Reset from previous analysis
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
#  Call with boundaries
MaxYValue = max(ESPVCallValue, ESPVCallLB, ESPVCallUB)
MinYValue = min(ESPVCallValue, ESPVCallLB, ESPVCallUB)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(StockPrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ESPVCallLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPVCallUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
#  Call time value
PVCallTV <- ESPVCallValue - ESPVCallLB
MaxValue = max(PVCallTV)
MinValue = min(PVCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "European-Style Binomial Option Time Value (ABM)"
xTitle = "Stock Price"
yTitle = "Call Time Value"
plot(StockPrice, PVCallTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Put with boundaries
MaxYValue = max(ESPVPutValue, ESPVPutLB, ESPVPutUB)
MinYValue = min(ESPVPutValue, ESPVPutLB, ESPVPutUB)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(StockPrice, ESPVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ESPVPutLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, ESPVPutUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
#  Put time value
PVPutTV <- ESPVPutValue - ESPVPutLB
MaxValue = max(PVPutTV)
MinValue = min(PVPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "European-Style Binomial Option Time Value (ABM)"
xTitle = "Stock Price"
yTitle = "Put Time Value"
plot(StockPrice, PVPutTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Calls and puts
MaxYValue = max(ESPVCallValue, ESPVPutValue)
MinYValue = min(ESPVCallValue, ESPVPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Option Value (ABM)"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, ESPVPutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
# Digital Options with respect to the Stock Price
MaxYValue = max(DigitalCallValue, DigitalPutValue)
MinYValue = min(DigitalCallValue, DigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(StockPrice); MinXValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Digital Option Value (ABM)"
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
ESPVCallValue <- numeric(StepCount)
ESPVPutValue <- numeric(StepCount)
DigitalCallValue <- numeric(StepCount)
DigitalPutValue <- numeric(StepCount)
j=0L
for(i in seq(MinStep, MaxStep, StepSize)){
  j = j + 1
  BINInputData$NumberOfSteps = as.integer(i)
  Value <- ABMESOptionValue(BINInputData)
  ESPVCallValue[j] = Value$CallValue
  ESPVPutValue[j] = Value$PutValue
  DigitalCallValue[j] = Value$DigitalCallValue
  DigitalPutValue[j] = Value$DigitalPutValue
}
BINInputData$NumberOfSteps = inputNumberOfSteps
# Plain vanilla option values with respect to the number of steps
xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
MaxOptionValue = max(ESPVCallValue,ESPVPutValue)
MinOptionValue = min(ESPVCallValue,ESPVPutValue)
ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Plain Vanilla Option Values (ABM)"
xTitle = "Number of Steps"
yTitle = "Option Value"
lTitle = "Parameter"
plot(NumberOfSteps, ESPVPutValue, type="b", main=mTitle, sub=sTitleBIN,
  xlab=xTitle, ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1)
lines(NumberOfSteps, ESPVCallValue, col="black", xlim=xlim1, 
  ylim=ylim1, pch = 2, type="b")
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
# Digital option values with respect to the number of steps
MaxOptionValue = max(DigitalCallValue, DigitalPutValue)
MinOptionValue = min(DigitalCallValue, DigitalPutValue)
ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
legtxt = c("Call Value","Put Value")
mTitle = "European-Style Binomial Digital Option Value (ABM)"
xTitle = "Number of Steps"
yTitle = "Digital Option Value"
lTitle = "Parameter"
plot(NumberOfSteps, DigitalCallValue, type="b", main=mTitle, sub=sTitleBINDP,
  xlab=xTitle, ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1,  pch = 1)
lines(NumberOfSteps, DigitalPutValue, col="black", xlim = xlim1, ylim = ylim1,
  type="b", pch = 2)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1), 
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program
