# GBM Sensitivity Analysis wrt Stock Price.R
# # Plots with StockPrice
StepSize = (SUpperBound - SLowerBound)/(NumberOfObservations - 1)
StockPrice <- c(1:NumberOfObservations)
CoCLowerBound <- c(1:NumberOfObservations)
CoPLowerBound <- c(1:NumberOfObservations)
PoCLowerBound <- c(1:NumberOfObservations)
PoPLowerBound <- c(1:NumberOfObservations)
CoCUpperBound <- c(1:NumberOfObservations)
CoPUpperBound <- c(1:NumberOfObservations)
PoCUpperBound <- c(1:NumberOfObservations)
PoPUpperBound <- c(1:NumberOfObservations)
CoCValue <- c(1:NumberOfObservations)
CoPValue <- c(1:NumberOfObservations)
PoCValue <- c(1:NumberOfObservations)
PoPValue <- c(1:NumberOfObservations)
CoCTV <- c(1:NumberOfObservations) # Time Value
CoPTV <- c(1:NumberOfObservations)
PoCTV <- c(1:NumberOfObservations)
PoPTV <- c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  StockPrice[i] <-SLowerBound + (i - 1)*StepSize
  COInputData$S <- StockPrice[i]
  COInputData$iC <- 1
  COInputData$iU <- 1
  COInputData$XC <- UCallValue
  CoCLowerBound[i] <- COLowerBound(COInputData)
  CoCUpperBound[i] <- COUpperBound(COInputData)
  CoCValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  CoCTV[i] <- CoCValue[i] - CoCLowerBound[i]
  COInputData$iC <- 1
  COInputData$iU <- -1
  COInputData$XC <- UPutValue
  CoPLowerBound[i] <- COLowerBound(COInputData)
  CoPUpperBound[i] <- COUpperBound(COInputData)
  CoPValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  CoPTV[i] <- CoPValue[i] - CoPLowerBound[i]
  COInputData$iC <- -1
  COInputData$iU <- 1
  COInputData$XC <- UCallValue
  PoCLowerBound[i] <- COLowerBound(COInputData)
  PoCUpperBound[i] <- COUpperBound(COInputData)
  PoCValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  PoCTV[i] <- PoCValue[i] - PoCLowerBound[i]
  COInputData$iC <- -1
  COInputData$iU <- -1
  COInputData$XC <- UPutValue
  PoPLowerBound[i] <- COLowerBound(COInputData)
  PoPUpperBound[i] <- COUpperBound(COInputData)
  PoPValue[i] <- COValue(COInputData, LowerBound, UpperBound) 
  PoPTV[i] <- PoPValue[i] - PoPLowerBound[i]
}
# Reset input values
COInputData$S = inputUnderlying
COInputData$iC <- 1
COInputData$iU <- 1
# Footer: Generic
TXU = paste0(' XU=', COInputData$XU)
TR = paste0(', r=', round(COInputData$r,4))
TQ = paste0(', q=', round(COInputData$q,4))
TD = paste0(', d=', round(COInputData$d,4))
TTU = paste0(', TU=', COInputData$TU)
TTC = paste0(', TC=', COInputData$TC)
TSig = paste0(', Vol=', COInputData$v)
lTitle = "Parameter"# x-axis
#
# CoC
#
TUO = paste0('UO=', round(UCallValue,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range: Find break point on x-axis
z <- -99
for(i in 2:NumberOfObservations){
  if(CoCLowerBound[i] > CoCLowerBound[i-1] && z < 0){
    MidPoint <- StockPrice[i]
    z <- 99
  }  
}
Range <- 0.5
MaxXValue = MidPoint * (1 + Range)
MinXValue = MidPoint * (1 - Range)
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
# Set y range: Find break point on y-axis
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){ # S just greater than S(Max)
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){ # S just less than S(Min)
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = max(CoCUpperBound[MaxXi])
MinYValue = min(CoCLowerBound[MinXi])
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("CoC Value","CoC Lower Bound", "CoC Upper Bound")
mTitle = "Call on Call Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, CoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CoCLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CoCUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Time Value Plot
MaxYValue = max(CoCTV)
MinYValue = 0.0
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
mTitle = "Call on Call Option Time Value"
xTitle = "Stock Price"
yTitle = "Time Values"
plot(StockPrice, CoCTV, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
#
# CoP
#
TUO = paste0('UO=', round(UPutValue,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
  if(CoPLowerBound[i] == 0 && z < 0){
    MidPoint <- StockPrice[i-2]
    z <- 99
  }  
}
Range <- 0.5
MaxXValue = MidPoint * (1 + Range)
MinXValue = MidPoint * (1 - Range)
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = CoPUpperBound[MinXi]
MinYValue = CoPLowerBound[MaxXi]
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("CoP Value","CoP Lower Bound", "CoP Upper Bound")
mTitle = "Call on Put Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, CoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, CoPLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, CoPUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("top", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Time Value Plot
MaxYValue = max(CoPTV)
MinYValue = 0.0
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
mTitle = "Call on Put Option Time Value"
xTitle = "Stock Price"
yTitle = "Time Values"
plot(StockPrice, CoPTV, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
#
# PoC
#
TUO = paste0('UO=', round(UCallValue,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
# Center where boundary hits zero
  if(PoCLowerBound[i] == 0 && z < 0){
      MidPoint <- StockPrice[i-1]
    z <- 99
  }  
}
Range <- 0.5
MaxXValue = MidPoint * (1 + Range)
MinXValue = MidPoint * (1 - Range)
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = max(PoCUpperBound[MinXi])
MinYValue = min(PoCLowerBound[MaxXi])
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("PoC Value","PoC Lower Bound", "PoC Upper Bound")
mTitle = "Put on Call Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, PoCValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PoCLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PoCUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomleft", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Time Value Plot
MaxYValue = max(PoCTV)
MinYValue = 0.0
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
mTitle = "Put on Call Option Time Value"
xTitle = "Stock Price"
yTitle = "Time Values"
plot(StockPrice, PoCTV, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
#
# PoP
#
TUO = paste0('UO=', round(UPutValue,2))
sTitle = paste0(TUO, TXU, TR, TQ, TD, TTU, TTC, TSig)
# Set x range
z <- -99
for(i in 2:NumberOfObservations){
  if(PoPLowerBound[i] > PoPLowerBound[i-1] && z < 0){
    MidPoint <- StockPrice[i-1]
    z <- 99
  }  
}
Range <- 0.5
MaxXValue = MidPoint * (1 + Range)
MinXValue = MidPoint * (1 - Range)
xlim1 = c(1:2)
xlim1[1] = MinXValue
xlim1[2] = MaxXValue
Maxz <- -99
Minz <- -99
for(i in 1:NumberOfObservations){
  if(StockPrice[i] > MaxXValue && Maxz < 0){
    MaxXi <- i
    Maxz <- 99
  }  
  if(StockPrice[i] > MinXValue && Minz < 0){
    MinXi <- i
    Minz <- 99
  }  
}
# Value Plot
MaxYValue = PoPUpperBound[MaxXi]
MinYValue = PoPLowerBound[MinXi]
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
legtxt = c("PoP Value","PoP Lower Bound", "PoP Upper Bound")
mTitle = "Put on Put Option Values and Boundaries"
xTitle = "Stock Price"
yTitle = "Values"
plot(StockPrice, PoPValue, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(StockPrice, PoPLowerBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
lines(StockPrice, PoPUpperBound, type = "p", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(0,0,0),
  col = c("black","black","black"), pch = c(1,2,3), bty = "n",
  title = lTitle)
# Time Value Plot
MaxYValue = max(PoPTV)
MinYValue = 0.0
ylim1 = c(1:2)
ylim1[1] = MinYValue
ylim1[2] = MaxYValue
mTitle = "Put on Put Option Time Value"
xTitle = "Stock Price"
yTitle = "Time Values"
plot(StockPrice, PoPTV, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
