# 5.5 ABMOVM Test.R
# Arithmetic Brownian Motion Option Valuation Model
# rmarkdown::render("5.5 ABMOVM Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Generic test inputs
inputStockPrice = 100.0
inputStrikePrice = 100.0
inputInterestRate = 5.0        # In percent
inputDividendYield = 0.0       # In percent
inputVolatility = 29.88 #476829  # In dollars, annualized
inputTimeToMaturity = 1
inputType = 1 # 1 for call, -1 for put
NumberOfObservations = 200
LowerBound = 0.5*inputStockPrice # Note centering on original values
UpperBound = 1.5*inputStockPrice
LowerBoundNL = 0.9*inputStockPrice # Note centering on original values
UpperBoundNL = 1.1*inputStockPrice
# Plot footers
TS = paste0('S=', inputStockPrice)
TX = paste0(',X=', inputStrikePrice)
TR = paste0(',r=', inputInterestRate)
Td = paste0(',d=', inputDividendYield, ',')
TV = paste0('Vol=', inputVolatility)
TT = paste0(',T=',inputTimeToMaturity)
sTitle = paste0(TS, TX, TR, Td, TV, TT)
#
# Available functions
#
#  ABMInputData - list of inputs with associated names
#  PV1(Maturity, Rate) - present value of $1
#  B = ABMInputData
#  d1(B) - value of d1
#  d2(B) - value of d2
#  n(d) - standard normal PDF, given scalar d
#  N(d) - standard normal CDF, given scalar d
#  ABMOptionValue(B) - option value, type = 1 is call, type = -1 is put
#  OptionLowerBound(B) - option lower bounds
#  OptionUpperBound(B) - option upper bounds
#
#  Input matrix 
#  ABMInputData - list of inputs with associated names
ABMInputData <- list(inputStockPrice, inputStrikePrice, inputInterestRate, 
  inputDividendYield, inputVolatility, inputTimeToMaturity, inputType)
names(ABMInputData) <- c("StockPrice", "StrikePrice", "InterestRate", 
  "DividendYield", "Volatility", "TimeToMaturity", "Type")
source("ABMOVM Functions.R")
#
# Test functions
#
# Basic functions
UIValue = ABMInputData$StockPrice
TestPV1 = PV1(inputTimeToMaturity, inputInterestRate)
Testdn = dn(ABMInputData)
Testn = n(dn(ABMInputData))
TestN = N(dn(ABMInputData))
Testdn; Testn; TestN
# Boundaries
ABMInputData$Type = 1
CallLowerBound = OptionLowerBound(ABMInputData)
CallUpperBound = OptionUpperBound(ABMInputData)
CallValue = ABMOptionValue(ABMInputData)
CallLowerBound; CallUpperBound; CallValue
ABMInputData$Type = -1
PutLowerBound = OptionLowerBound(ABMInputData)
PutUpperBound = OptionUpperBound(ABMInputData)
PutValue = ABMOptionValue(ABMInputData)
PutLowerBound; PutUpperBound; PutValue
#
# Illustrations with plots
#
StepSize = (UpperBound - LowerBound)/NumberOfObservations
StockPrice <- c(1:NumberOfObservations)
CallValue <- c(1:NumberOfObservations)
PutValue <- c(1:NumberOfObservations)
CallLowerBound <- c(1:NumberOfObservations)
PutLowerBound <- c(1:NumberOfObservations)
CallUpperBound <- c(1:NumberOfObservations)
PutUpperBound <- c(1:NumberOfObservations)
CallTimeValue <- c(1:NumberOfObservations)
PutTimeValue <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <- as.double(LowerBound + (i-1)*StepSize)
  ABMInputData$StockPrice = StockPrice[i]
  ABMInputData$Type = 1
  CallLowerBound[i] <- OptionLowerBound(ABMInputData)
  CallUpperBound[i] <- OptionUpperBound(ABMInputData)
  CallValue[i] <- ABMOptionValue(ABMInputData)
  CallTimeValue[i] <- CallValue[i] - CallLowerBound[i]
  ABMInputData$Type = -1
  PutLowerBound[i] <- OptionLowerBound(ABMInputData)
  PutUpperBound[i] <- OptionUpperBound(ABMInputData)
  PutValue[i] <- ABMOptionValue(ABMInputData)
  PutTimeValue[i] <- PutValue[i] - PutLowerBound[i]
}
ABMInputData$StockPrice = inputStockPrice
# Option value plots
MaxValue = max(CallValue, CallLowerBound)
MinValue = min(CallValue, CallLowerBound)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Lower Bound")
mTitle = "ABM Call Value"
xTitle = "Stock Price"
yTitle = "Call Value"
lTitle = "Parameter"
plot(StockPrice, CallValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CallLowerBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# Put value
MaxValue = max(PutValue, PutLowerBound)
MinValue = min(PutValue, PutLowerBound)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Put Value","Lower Bound")
mTitle = "ABM Put Value"
xTitle = "Stock Price"
yTitle = "Put Value"
lTitle = "Parameter"
plot(StockPrice, PutValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutLowerBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
  col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# All together now
MaxValue = max(CallValue, CallLowerBound, PutValue, PutLowerBound)
MinValue = min(CallValue, CallLowerBound, PutValue, PutLowerBound)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound", "Put Value", "Put Lower Bound")
mTitle = "ABM Option Values"
xTitle = "Stock Price"
yTitle = "Option Value"
lTitle = "Parameter"
plot(StockPrice, CallValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CallLowerBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PutValue, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
lines(StockPrice, PutLowerBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 4, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(1, 1, 1, 1),
  col = c("black","black","black","black"), pch = c(1, 2, 3, 4),
  bty = "n", title = lTitle)
#
# Option time value plots
#
MaxValue = max(CallTimeValue); MinValue = min(CallTimeValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value","Lower Bound")
mTitle = "ABM Call Time Value"
xTitle = "Stock Price"
yTitle = "Call Time Value"
lTitle = "Parameter"
plot(StockPrice, CallTimeValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Put time value
MaxValue = max(PutTimeValue); MinValue = min(PutTimeValue)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Put Value","Lower Bound")
mTitle = "ABM Put Time Value"
xTitle = "Stock Price"
yTitle = "Put Time Value"
lTitle = "Parameter"
plot(StockPrice, PutTimeValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
#
# Option boundary plots
#
MaxValue = max(CallValue, CallLowerBound, CallUpperBound)
MinValue = min(CallValue, CallLowerBound, CallUpperBound)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Call Value", "Call Lower Bound", "Call Upper Bound")
mTitle = "ABM Option Values"
xTitle = "Stock Price"
yTitle = "Value"
lTitle = "Parameter"
plot(StockPrice, CallValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CallLowerBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CallUpperBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", title = lTitle)
# Put bounds
MaxValue = max(PutValue, PutLowerBound, PutUpperBound)
MinValue = min(PutValue, PutLowerBound, PutUpperBound)
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(StockPrice); MinValue = min(StockPrice)
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
legtxt = c("Put Value", "Put Lower Bound", "Put Upper Bound")
mTitle = "ABM Option Values"
xTitle = "Stock Price"
yTitle = "Value"
lTitle = "Parameter"
plot(StockPrice, PutValue, type = "b", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PutLowerBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PutUpperBound, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", title = lTitle)
# #
# # Open a pdf file
# #
# pdf("Put Values w Boundaries.pdf")
# plot(StockPrice, PutValue, type = "b", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(StockPrice, PutLowerBound, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# lines(StockPrice, PutUpperBound, type = "b", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 3, cex = 0.5)
# legend("bottomleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
#   col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", title = lTitle)
# #
# # Just messing around - log scale
# #
# StrikePrice <- c(1:NumberOfObservations)
# StepSize = (UpperBoundNL - LowerBoundNL)/NumberOfObservations
# for(i in 1:NumberOfObservations){
#   StrikePrice[i] <- as.double(LowerBoundNL + (i-1)*StepSize)
#   ABMInputData$StrikePrice = StrikePrice[i]
#   ABMInputData$Type = 1
#   CallLowerBound[i] <- OptionLowerBound(ABMInputData)
#   CallUpperBound[i] <- OptionUpperBound(ABMInputData)
#   CallValue[i] <- ABMOptionValue(ABMInputData)
#   CallTimeValue[i] <- CallValue[i] - CallLowerBound[i]
#   ABMInputData$Type = -1
#   PutLowerBound[i] <- OptionLowerBound(ABMInputData)
#   PutUpperBound[i] <- OptionUpperBound(ABMInputData)
#   PutValue[i] <- ABMOptionValue(ABMInputData)
#   PutTimeValue[i] <- PutValue[i] - PutLowerBound[i]
# }
# ABMInputData$StrikePrice = inputStrikePrice
# 
# yC <- 100*log(1+CallTimeValue/ABMInputData$StockPrice)
# yP <- 100*log(1+PutTimeValue/ABMInputData$StockPrice)
# x <- StrikePrice/ABMInputData$StockPrice
# MaxValuey = max(yC, yP); MinValuey = min(yC, yP)
# ylim1 = c(1:2); ylim1[1] = MinValuey; ylim1[2] = MaxValuey
# MaxValuex = max(x); MinValuex = min(x)
# xlim1 = c(1:2); xlim1[1] = MinValuex; xlim1[2] = MaxValuex
# legtxt = c("Call Value", "Put Value")
# mTitle = "Nat. Log 1 + Option Time Value to Stock Price"
# xTitle = "Strike Price/Stock Price"
# yTitle = "Nat. Log (1 + TV/S)"
# lTitle = "Parameter"
# plot(x, yC, type = "p", main = mTitle,
#   sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
#   ylim = ylim1, pch = 1, cex = 0.5)
# lines(x, yP, type = "p", col ="black", xlim = xlim1,
#   ylim = ylim1, pch = 2, cex = 0.5)
# legend("top", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
#   col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
# 
