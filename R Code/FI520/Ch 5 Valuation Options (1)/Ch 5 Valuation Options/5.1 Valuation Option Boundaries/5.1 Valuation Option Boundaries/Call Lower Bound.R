# Call Lower Bound.R
OptionType = 1   # 1 for call, 0 for put, 2 for both
TempData <- read.xlsx(xlsxFile = FileName, sheet = 1, 
  skipEmptyRows=FALSE)
LengthTempData <- length(TempData$date)
for(i in 1:LengthTempData){
  if(TempData$cp_flag[i] == "C"){
    TempData$IntrinsicValue[i] <- max(0, TempData$MidPrice[i] - 
      TempData$StrikePrice[i])
    TempData$LowerBound[i] <- max(0, TempData$MidPrice[i] - 
      TempData$PVDiv[i] - TempData$StrikePrice[i] * 
        exp(-(TempData$Rate[i]/100)*TempData$TTM[i]))
    TempData$TimeValue[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$IntrinsicValue[i])
  }
  if(TempData$cp_flag[i] == "P"){
    TempData$IntrinsicValue[i] <- max(0, TempData$StrikePrice[i] - 
      TempData$MidPrice[i])
    TempData$LowerBound[i] <- max(0, TempData$StrikePrice[i] * 
      exp(-(TempData$Rate[i]/100)*TempData$TTM[i]) + TempData$PVDiv[i] 
      - TempData$MidPrice[i])
    TempData$TimeValue[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$IntrinsicValue[i])
  }
}
# Plot footers
TS = paste0('S=', round(TempData$MidPrice[1],2))
TT = paste0(',T=', TempData$DTM[1],' Days')
TR = paste0(',R=', round(TempData$Rate[1],2))
Td = paste0(',d=', round((TempData$PVDiv[1]/TempData$MidPrice[1]) * 
  100*(365.25/TempData$DTM[1]),2))
# TD = paste0(',', format(as.Date(TempData$JDate[1], origin = "1970-01-01"),
#   "%m/%d"), "/20XX")
TD = paste0(',', format(as.Date(TempData$JDate[1], origin = "1970-01-01"),
  "%m/%d/%y"))
sTitle = paste0(TS, TT, TR, Td, TD)
# Option value plots wrt strike price
if(OptionType == 1){
  TempDataC <- TempData[TempData$cp_flag == 'C',]
  TempDataC <- TempDataC[order(TempDataC$StrikePrice),]
  xC1 <- 100*TempDataC$StrikePrice/TempDataC$MidPrice
  yCIV <- 100*TempDataC$IntrinsicValue/TempDataC$MidPrice
  yCLB <- 100*TempDataC$LowerBound/TempDataC$MidPrice
  yCOP <- 100*TempDataC$OptionPrice/TempDataC$MidPrice
  yCTV <- 100*TempDataC$TimeValue/TempDataC$MidPrice
  MaxValuey = max(yCIV, yCLB, yCOP)
  MinValuey = min(yCIV, yCLB, yCOP)
  MaxValuex = max(xC1); MinValuex = min(xC1)
  MaxValuey2 = max(yCTV); MinValuey2 = min(yCTV)
}
ylim1 = c(1:2); ylim1[1] = MinValuey; ylim1[2] = MaxValuey
xlim1 = c(1:2); xlim1[1] = MinValuex; xlim1[2] = MaxValuex
if(FixXRangeCLB){
  xlim1[1] <- MinXRangeCLB
  xlim1[2] <- MaxXRangeCLB
}
# Plot call lower bound
if(CallLB){
  if(FixYRangeCLB){
    ylim1[1] <- MinYRangeCLB
    ylim1[2] <- MaxYRangeCLB
  }
  yTitle = "Value/Stock Price"
  xTitle = "Strike Price/StockPrice"
  lTitle = "Parameter"
  mTitle = "Call Options As Percentage of Stock Price"
  legtxt = c("Call Mid Price", "Call Intrinsic Value", "ES Call Lower Bound")
  plot(xC1, yCOP, type = "b", main = mTitle,
    sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
    ylim = ylim1, pch = 1, cex = 0.5)
  lines(xC1, yCIV, type = "b", col ="black", xlim = xlim1,
    ylim = ylim1, pch = 2, cex = 0.5)
  lines(xC1, yCLB, type = "b", col ="black", xlim = xlim1,
    ylim = ylim1, pch = 3, cex = 0.5)
  legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
    col = c("black","black", "black"), pch = c(1, 2, 3), bty = "n", 
    title = lTitle)
}
# Plot call Time Value
if(CallTV){
  ylim1 = c(1:2); ylim1[1] = MinValuey2; ylim1[2] = MaxValuey2
  if(FixXRangeCTV){
    xlim1[1] <- MinXRangeCTV
    xlim1[2] <- MaxXRangeCTV
  }
  if(FixYRangeCTV){
    ylim1[1] <- MinYRangeCTV
    ylim1[2] <- MaxYRangeCTV
  }
  yTitle = "Value/Stock Price"
  xTitle = "Strike Price/StockPrice"
  mTitle = "Call Time Value As Percentage of Stock Price"
  plot(xC1, yCTV, type = "b", main = mTitle,
    sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
    ylim = ylim1, pch = 1, cex = 0.5)
}

