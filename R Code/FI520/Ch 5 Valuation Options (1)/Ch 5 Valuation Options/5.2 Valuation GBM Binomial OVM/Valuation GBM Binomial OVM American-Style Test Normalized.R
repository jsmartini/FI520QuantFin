# Valuation GBM Binomial OVM American-Style Normalized Test.R
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
source('ASGBMBINOVM Functions.R')
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
StepSize = 5L # Analysis wrt number of steps
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
BINInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate,
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType,
  inputNumberOfSteps, inputPayoutType, inputEMMProbability,
  inputDigitalPayout)
names(BINInputData) <- c("StockPrice", "StrikePrice", "InterestRate",
  "DividendYield", "Volatility", "TimeToMaturity", "Type", "NumberOfSteps",
  "PayoutType", "EMMProbability", "DigitalPayout")
BINInputData
# Test functions
BINInputData$Type = 1L
BINInputData$PayoutType = 1L
ESCallValue = ESBINOptionValue(BINInputData)
ASCallValue = ASBINOptionValue(BINInputData)
BINInputData$Type = -1L
ESPutValue = ESBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = 1L
ESDigitalCallValue = ESBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = -1L
ESDigitalPutValue = ESBINOptionValue(BINInputData)
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
DV = ESDigitalCallValue + ESDigitalPutValue
ESDiff = PV - DV
ESCallValue; ESPutValue; ESDigitalCallValue; ESDigitalPutValue; DV; ESDiff
BINInputData$Type = 1L
BINInputData$PayoutType = 1L
ASCallValue = ASBINOptionValue(BINInputData)
BINInputData$Type = -1L
ASPutValue = ASBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = 1L
ASDigitalCallValue = ASBINOptionValue(BINInputData)
BINInputData$PayoutType = 2L
BINInputData$Type = -1L
ASDigitalPutValue = ASBINOptionValue(BINInputData)
PV = exp(-(inputInterestRate/100.0)*inputTimeToMaturity)*inputDigitalPayout
DV = ASDigitalCallValue + ASDigitalPutValue
ASDiff = PV - DV
ASCallValue; ASPutValue; ASDigitalCallValue; ASDigitalPutValue; DV; ASDiff
# Normalized Strike Price
StepSizeX = (UpperBoundX - LowerBoundX)/(NumberOfObservations - 1)
StrikePrice <- c(1:NumberOfObservations)
NStrikePrice <- c(1:NumberOfObservations)
ESPVCallLB <- c(1:NumberOfObservations)
ESPVCallUB <- c(1:NumberOfObservations)
ESPVCallValue <- c(1:NumberOfObservations)
ESPVPutLB <- c(1:NumberOfObservations)
ESPVPutUB <- c(1:NumberOfObservations)
ESPVPutValue <- c(1:NumberOfObservations)
ASPVCallLB <- c(1:NumberOfObservations)
ASPVCallUB <- c(1:NumberOfObservations)
ASPVCallValue <- c(1:NumberOfObservations)
ASPVPutLB <- c(1:NumberOfObservations)
ASPVPutUB <- c(1:NumberOfObservations)
ASPVPutValue <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StrikePrice[i] <- LowerBoundX + (i - 1)*StepSizeX
  NStrikePrice[i] <- (StrikePrice[i]/inputStockPrice)*100 
# European-Style
  BINInputData$StrikePrice = StrikePrice[i]
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ESPVCallLB[i] = (ESBINOptionLowerBound(BINInputData)/inputStockPrice)*100
  ESPVCallUB[i] = (ESBINOptionUpperBound(BINInputData)/inputStockPrice)*100
  ESPVCallValue[i] = (ESBINOptionValue(BINInputData)/inputStockPrice)*100
  BINInputData$Type = -1L
  ESPVPutLB[i] = (ESBINOptionLowerBound(BINInputData)/inputStockPrice)*100
  ESPVPutUB[i] = (ESBINOptionUpperBound(BINInputData)/inputStockPrice)*100
  ESPVPutValue[i] = (ESBINOptionValue(BINInputData)/inputStockPrice)*100
# American-style
  BINInputData$Type = 1L
  BINInputData$PayoutType = 1L
  ASPVCallLB[i] = (ASBINOptionLowerBound(BINInputData)/inputStockPrice)*100
  ASPVCallUB[i] = (ASBINOptionUpperBound(BINInputData)/inputStockPrice)*100
  ASPVCallValue[i] = (ASBINOptionValue(BINInputData)/inputStockPrice)*100
  BINInputData$Type = -1L
  ASPVPutLB[i] = (ASBINOptionLowerBound(BINInputData)/inputStockPrice)*100
  ASPVPutUB[i] = (ASBINOptionUpperBound(BINInputData)/inputStockPrice)*100
  ASPVPutValue[i] = (ASBINOptionValue(BINInputData)/inputStockPrice)*100
}
BINInputData$StrikePrice = inputStrikePrice # Reset from previous analysis
# #
# # European-style only
# #
# # Plots: Plain Vanilla Options with respect to the normalized strike price
# MaxValue = max(NStrikePrice); MinValue = min(NStrikePrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# #  Call with boundaries
# MaxValue = max(ESPVCallValue, ESPVCallLB, ESPVCallUB)
# MinValue = min(ESPVCallValue, ESPVCallLB, ESPVCallUB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
# mTitle = "Normalized ES Binomial Call Option Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Call Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NStrikePrice, ESPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(NStrikePrice, ESPVCallLB, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(NStrikePrice, ESPVCallUB, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("black", "black", "black"), pch = c(1,2,3), bty = "n", 
#   title = lTitle)
# #  Call time value
# ESPVCallTV <- ESPVCallValue - ESPVCallLB
# MaxValue = max(ESPVCallTV)
# MinValue = min(ESPVCallTV)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# mTitle = "Normalized ES Binomial Call Option Time Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Call Time Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NStrikePrice, ESPVCallTV, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# # Put with boundaries
# MaxValue = max(ESPVPutValue, ESPVPutLB, ESPVPutUB)
# MinValue = min(ESPVPutValue, ESPVPutLB, ESPVPutUB)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
# mTitle = "Normalized ES Binomial Put Option Time Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Put Time Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NStrikePrice, ESPVPutValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(NStrikePrice, ESPVPutLB, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(NStrikePrice, ESPVPutUB, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("black", "black", "black"), pch = c(1,2,3), bty = "n", 
#   title = lTitle)
# #  Put time value
# ESPVPutTV <- ESPVPutValue - ESPVPutLB
# MaxValue = max(ESPVPutTV)
# MinValue = min(ESPVPutTV)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# mTitle = "Normalized ES Binomial Put Option Time Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Put Time Value/Stock Price (%)"
# plot(NStrikePrice, ESPVPutTV, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# # Calls and puts
# MaxValue = max(ESPVCallValue, ESPVPutValue)
# MinValue = min(ESPVCallValue, ESPVPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# MaxValue = max(NStrikePrice); MinValue = min(NStrikePrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# legtxt = c("Call Value","Put Value")
# mTitle = "Normalized ES Binomial Option Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Option Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NStrikePrice, ESPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(NStrikePrice, ESPVPutValue, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
#
# Contrast ES with AS
#
# Plots: ES and AS call and put values
MaxValue = max(ESPVCallValue, ESPVPutValue, ASPVCallValue, ASPVPutValue)
MinValue = min(ESPVCallValue, ESPVPutValue, ASPVCallValue, ASPVPutValue)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
MaxValue = max(NStrikePrice)
MinValue = min(NStrikePrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
legtxt = c("ESCall Value", "ASCall Value", "ESPut Value", "ASPut Value")
mTitle = "Normalized Binomial Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ASPVCallValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(NStrikePrice, ESPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(NStrikePrice, ASPVPutValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
# Plots: ES and AS call values and LB
MaxValue = max(ESPVCallValue, ESPVCallLB, ASPVCallValue, ASPVCallLB)
MinValue = min(ESPVCallValue, ESPVCallLB, ASPVCallValue, ASPVCallLB)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
MaxValue = max(NStrikePrice)
MinValue = min(NStrikePrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
legtxt = c("ES Call Value", "ES Call LB", "AS Call Value", "AS Call LB")
mTitle = "Normalized Binomial Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ESPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
lines(NStrikePrice, ASPVCallValue, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
lines(NStrikePrice, ASPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4), bty = "n", 
  title = lTitle)
#  ES call with lower bound
MaxValue = max(ESPVCallValue, ESPVCallLB)
MinValue = min(ESPVCallValue, ESPVCallLB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call Value", "ES Call Lower Bound")
mTitle = "Normalized ES Binomial Call Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Call Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ESPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ESPVCallLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black", "black"), pch = c(1,2), bty = "n",
  title = lTitle)
# Plots: AS call values and LB
MaxValue = max(ASPVCallValue, ASPVCallLB)
MinValue = min(ASPVCallValue, ASPVCallLB)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
MaxValue = max(NStrikePrice)
MinValue = min(NStrikePrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
legtxt = c("AS Call Value", "AS Call LB")
mTitle = "Normalized AS Binomial Call Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ASPVCallValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ASPVCallLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", 
  title = lTitle)
#  Call time value
ESCallTV <- ESPVCallValue - ESPVCallLB
ASCallTV <- ASPVCallValue - ASPVCallLB
MaxValue = max(ESCallTV, ASCallTV)
MinValue = min(ESCallTV, ASCallTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Call Time Value","AS Call Time Value")
mTitle = "Normalized Binomial Call Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ESCallTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ASCallTV, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
#  ES put with lower bound
MaxValue = max(ESPVPutValue, ESPVPutLB)
MinValue = min(ESPVPutValue, ESPVPutLB)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Put Value", "ES Put Lower Bound")
mTitle = "Normalized ES Binomial Put Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Put Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ESPVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ESPVPutLB, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black", "black"), pch = c(1, 2), bty = "n",
  title = lTitle)
# Plots: AS Put values and LB
MaxValue = max(ASPVPutValue, ASPVPutLB)
MinValue = min(ASPVPutValue, ASPVPutLB)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
MaxValue = max(NStrikePrice)
MinValue = min(NStrikePrice)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
legtxt = c("AS Put Value", "AS Put LB")
mTitle = "Normalized AS Binomial Put Option Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ASPVPutValue, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ASPVPutLB, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", 
  title = lTitle)
#  Put time value
ESPutTV <- ESPVPutValue - ESPVPutLB
ASPutTV <- ASPVPutValue - ASPVPutLB
MaxValue = max(ESPutTV, ASPutTV)
MinValue = min(ESPutTV, ASPutTV)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
legtxt = c("ES Put Time Value","AS Put Time Value")
mTitle = "Normalized Binomial Put Time Value (GBM)"
xTitle = "Strike Price/Stock Price (%)"
yTitle = "Option Value/Stock Price (%)"
lTitle = "Parameter"
plot(NStrikePrice, ESPutTV, type = "b", main = mTitle,
  sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(NStrikePrice, ASPutTV, type = "b", col ="black", 
  xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
legend("bottom", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# # Contrast AS call and puts
# MaxValue = max(ASPVCallValue, ASPVPutValue)
# MinValue = min(ASPVCallValue, ASPVPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# MaxValue = max(NStrikePrice); MinValue = min(NStrikePrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# legtxt = c("AS Call Value","AS Put Value")
# mTitle = "Normalized Binomial AS Option Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Option Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NStrikePrice, ASPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(NStrikePrice, ASPVPutValue, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# # AS- and ES call and put values
# MaxValue = max(ASPVCallValue, ASPVPutValue, ESPVCallValue, ESPVPutValue)
# MinValue = min(ASPVCallValue, ASPVPutValue, ESPVCallValue, ESPVPutValue)
# ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
# MaxValue = max(NStrikePrice); MinValue = min(NStrikePrice)
# xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
# legtxt = c("AS Call Value","AS Put Value", "ES Call Value","ES Put Value")
# mTitle = "Normalized Binomial Option Value (GBM)"
# xTitle = "Strike Price/Stock Price (%)"
# yTitle = "Option Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NStrikePrice, ASPVCallValue, type = "b", main = mTitle,
#   sub = sTitleBIN, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(NStrikePrice, ASPVPutValue, type = "b", col ="black", 
#   xlim = xlim1, ylim = ylim1, pch = 2, cex = 0.5)
# lines(NStrikePrice, ESPVCallValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 3, cex = 0.5)
# lines(NStrikePrice, ESPVPutValue, type = "b", col ="black",
#   xlim = xlim1, ylim = ylim1, pch = 4, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(1, 1, 1, 1),
#   col = c("black","black", "black", "black"), pch = c(1, 2, 3, 4), 
#   bty = "n", title = lTitle)
# #
# # Plots with Number Of Steps
# #
# StepCount = as.integer((as.numeric(MaxStep)-as.numeric(MinStep)) /
#   as.numeric(StepSize) + 1)
# NumberOfSteps = as.integer(seq(MinStep,MaxStep,StepSize))
# ASPVCallValue <- c(1:StepCount)
# ASPVPutValue <- c(1:StepCount)
# j=0L
# for(i in seq(MinStep, MaxStep, StepSize)){
#   j = j + 1
#   BINInputData$NumberOfSteps = as.integer(i)
#   BINInputData$Type = 1L
#   BINInputData$PayoutType = 1L
#   ASPVCallValue[j] = (ASBINOptionValue(BINInputData)/inputStockPrice)*100
#   BINInputData$Type = -1L
#   ASPVPutValue[j] = (ASBINOptionValue(BINInputData)/inputStockPrice)*100
# }
# ASBINInputData$NumberOfSteps = inputNumberOfSteps
# # Plain vanilla option values with respect to the number of steps
# xlim1 = c(1:2); xlim1[1] = MinStep; xlim1[2] = MaxStep
# MaxOptionValue = max(ASPVCallValue, ASPVPutValue)
# MinOptionValue = min(ASPVCallValue, ASPVPutValue)
# ylim1 = c(1:2); ylim1[1] = MinOptionValue; ylim1[2] = MaxOptionValue
# legtxt = c("Call Value","Put Value")
# mTitle = "Normalized AS Binomial Option Value (GBM)"
# xTitle = "Number of Steps"
# yTitle = "Option Value/Stock Price (%)"
# lTitle = "Parameter"
# plot(NumberOfSteps, ASPVCallValue, type="b", main=mTitle, sub=sTitleBIN,
#   xlab=xTitle, ylab=yTitle, col="black",
#   xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
# lines(NumberOfSteps, ASPVPutValue, type="b", col="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("right", legtxt, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)

beep(sound = 10, print('Finished')) # Helpful when running long program

