# Valuation GBM Binomial OVM European-Style Normalized Test.R
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
LowerBoundX = 50 # Analysis wrt strike price
UpperBoundX = 150
NumberOfObservations = 101
StepSize = 1L # Analysis wrt number of steps
MinStep = 5L # Must be multiple of StepSize
MaxStep = 500L # Must be multiple of StepSize
# Plot footers
TS = paste0('S=', inputStockPrice)
# TX = paste0(',X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
Td = paste0(',d=', inputDividendYield, ',')
TV = paste0('Vol=', inputVolatility)
TT = paste0(',T=',inputTimeToMaturity)
TN = paste0(',N=',inputNumberOfSteps)
TDP = paste0(',DP=', inputDigitalPayout)
sTitleBIN = paste0(TS, TR, Td, TV, TT, TN)
sTitleBINDP = paste0(TS, TR, Td, TV, TT, TN, TDP)
#  ESBINInputData - list of inputs with associated names
ESBINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType, 
  inputNumberOfSteps, inputPayoutType, inputEMMProbability, 
  inputDigitalPayout)
names(ESBINInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type", "NumberOfSteps", 
  "PayoutType", "EMMProbability", "DigitalPayout")
is.integer(ESBINInputData$Type)
ESBINInputData
ESBINInputData$Type = 1L
ESBINInputData$PayoutType = 1L
PVCallLB = ESBINOptionLowerBound(ESBINInputData) 
PVCallLB
PVCallUB = ESBINOptionUpperBound(ESBINInputData) 
PVCallUB
PVCallValue = ESBINOptionValue(ESBINInputData) 
PVCallValue
ESBINInputData$Type = -1L
PVPutLB = ESBINOptionLowerBound(ESBINInputData) 
PVPutLB
PVPutUB = ESBINOptionUpperBound(ESBINInputData) 
PVPutUB
PVPutValue = ESBINOptionValue(ESBINInputData) 
PVPutValue
ESBINInputData$PayoutType = 2L
ESBINInputData$Type = 1L
DigitalCallValue = ESBINOptionValue(ESBINInputData) 
DigitalCallValue
ESBINInputData$PayoutType = 2L
ESBINInputData$Type = -1L
DigitalPutValue = ESBINOptionValue(ESBINInputData) 
DigitalPutValue
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
PV
DV = DigitalCallValue + DigitalPutValue
DV
Diff = PV - DV
Diff
# Normalized Strike Price
StepSizeX = (UpperBoundX - LowerBoundX)/(NumberOfObservations - 1)
StrikePrice <- c(1:NumberOfObservations)
NStrikePrice <- c(1:NumberOfObservations)
PVCallLB <- c(1:NumberOfObservations)
PVCallUB <- c(1:NumberOfObservations)
PVCallValue <- c(1:NumberOfObservations)
PVPutLB <- c(1:NumberOfObservations)
PVPutUB <- c(1:NumberOfObservations)
PVPutValue <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StrikePrice[i] <- LowerBoundX + (i - 1)*StepSizeX
  NStrikePrice[i] <- (StrikePrice[i]/inputStockPrice)*100 
  ESBINInputData$StrikePrice = StrikePrice[i]
  ESBINInputData$Type = 1L
  ESBINInputData$PayoutType = 1L
  PVCallLB[i] = (ESBINOptionLowerBound(ESBINInputData)/inputStockPrice)*100
  PVCallUB[i] = (ESBINOptionUpperBound(ESBINInputData)/inputStockPrice)*100
  PVCallValue[i] = (ESBINOptionValue(ESBINInputData)/inputStockPrice)*100
  ESBINInputData$Type = -1L
  PVPutLB[i] = (ESBINOptionLowerBound(ESBINInputData)/inputStockPrice)*100
  PVPutUB[i] = (ESBINOptionUpperBound(ESBINInputData)/inputStockPrice)*100
  PVPutValue[i] = (ESBINOptionValue(ESBINInputData)/inputStockPrice)*100
}
ESBINInputData$StrikePrice = inputStrikePrice # Reset from previous analysis
# Plots: Plain Vanilla Options with respect to the Stock Price
MaxValue = max(NStrikePrice); MinValue = min(NStrikePrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
#  Call with boundaries
MaxValue = max(PVCallValue, PVCallLB, PVCallUB)
MinValue = min(PVCallValue, PVCallLB, PVCallUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
mTitle = "Normalized ES Binomial Call Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Call Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, PVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, PVCallLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(NStrikePrice, PVCallUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#  Call time value
PVCallTV <- PVCallValue - PVCallLB
MaxValue = max(PVCallTV)
MinValue = min(PVCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "Normalized ES Binomial Call Option Time Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Call Time Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, PVCallTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Put with boundaries
MaxValue = max(PVPutValue, PVPutLB, PVPutUB)
MinValue = min(PVPutValue, PVPutLB, PVPutUB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
mTitle = "Normalized ES Binomial Put Option Time Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Put Time Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, PVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, PVPutLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(NStrikePrice, PVPutUB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black", "black", "black"), pch = c(1,2,3), bty = "n", 
  title = lTitle)
#  Put time value
PVPutTV <- PVPutValue - PVPutLB
MaxValue = max(PVPutTV)
MinValue = min(PVPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
mTitle = "Normalized ES Binomial Put Option Time Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Put Time Value/Stock Price (%)"
plot(NStrikePrice, PVPutTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Calls and puts
MaxValue = max(PVCallValue, PVPutValue)
MinValue = min(PVCallValue, PVPutValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(NStrikePrice); MinValue = min(NStrikePrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Put Value")
mTitle = "Normalized ES Binomial Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, PVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, PVPutValue, type = "b", col ="black", xlim = xlim1,
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
j=0L
for(i in seq(MinStep, MaxStep, StepSize)){
  j = j + 1
  ESBINInputData$NumberOfSteps = as.integer(i)
  ESBINInputData$Type = 1L
  ESBINInputData$PayoutType = 1L
  # PVCallValue[j] = i
  PVCallValue[j] = (ESBINOptionValue(ESBINInputData)/inputStockPrice)*100
  ESBINInputData$Type = -1L
  PVPutValue[j] = (ESBINOptionValue(ESBINInputData)/inputStockPrice)*100
}
ESBINInputData$NumberOfSteps = inputNumberOfSteps
# Plain vanilla option values with respect to the number of steps
xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
MaxOptionValue = max(PVCallValue, PVPutValue)
MinOptionValue = min(PVCallValue, PVPutValue)
ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
legtxt = c("Call Value","Put Value")
mTitle = "Normalized ES Binomial Option Value (GBM)"
xTitle = "Number of Steps"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NumberOfSteps, PVCallValue, type="b", main=mTitle, sub=sTitleBIN,
  xlab=xTitle, ylab=yTitle, col="black",
  xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
lines(NumberOfSteps, PVPutValue, type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("right", legtxt, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program

