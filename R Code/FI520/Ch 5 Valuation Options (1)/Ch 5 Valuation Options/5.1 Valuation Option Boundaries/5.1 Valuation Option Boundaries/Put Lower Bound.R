# Put Lower Bound.R
OptionType = 0   # 1 for call, 0 for put, 2 for both
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
    TempData$LowerBound[i] <- max(TempData$IntrinsicValue[i], 
      TempData$LowerBound[i]) 
    TempData$TimeValue[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$IntrinsicValue[i])
  }
  if(TempData$cp_flag[i] == "P"){
    TempData$IntrinsicValue[i] <- max(0, TempData$StrikePrice[i] - 
      TempData$MidPrice[i])
    TempData$LowerBound[i] <- max(0, TempData$StrikePrice[i] *
      exp(-(TempData$Rate[i]/100)*TempData$TTM[i]) + TempData$PVDiv[i] - 
      TempData$MidPrice[i])
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
if(OptionType == 0){
  TempDataP <- TempData[TempData$cp_flag == 'P',]
  TempDataP <- TempDataP[order(TempDataP$StrikePrice),]
  xP1 <- 100*TempDataP$StrikePrice/TempDataP$MidPrice
  yPIV <- 100*TempDataP$IntrinsicValue/TempDataP$MidPrice
  yPLB <- 100*TempDataP$LowerBound/TempDataP$MidPrice
  yPOP <- 100*TempDataP$OptionPrice/TempDataP$MidPrice
  yPTV <- 100*TempDataP$TimeValue/TempDataP$MidPrice
  MaxValuey = max(yPIV, yPLB, yPOP)
  MinValuey = min(yPIV, yPLB, yPOP)
  MaxValuex = max(xP1); MinValuex = min(xP1)
  MaxValuey2 = max(yPTV); MinValuey2 = min(yPTV)
}
ylim1 = c(1:2); ylim1[1] = MinValuey; ylim1[2] = MaxValuey
xlim1 = c(1:2); xlim1[1] = MinValuex; xlim1[2] = MaxValuex
if(FixXRangePLB){
  xlim1[1] <- MinXRangePLB
  xlim1[2] <- MaxXRangePLB
}
# Put lower bound
if(PutLB){
  if(FixYRangePLB){
    ylim1[1] <- MinYRangePLB
    ylim1[2] <- MaxYRangePLB
  }
  yTitle = "Value/Stock Price"
  xTitle = "Strike Price/StockPrice"
  lTitle = "Parameter"
  mTitle = "Put Options As Percentage of Stock Price"
  legtxt = c("Put Mid Price", "Put Intrinsic Value", "ES Put Lower Bound")
  plot(xP1, yPOP, type = "b", main = mTitle,
    sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
    ylim = ylim1, pch = 1, cex = 0.5)
  lines(xP1, yPIV, type = "b", col ="black", xlim = xlim1,
    ylim = ylim1, pch = 2, cex = 0.5)
  lines(xP1, yPLB, type = "b", col ="black", xlim = xlim1,
    ylim = ylim1, pch = 3, cex = 0.5)
  legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
    col = c("black","black", "black"), pch = c(1, 2, 3), bty = "n", 
    title = lTitle)
}
# Put time value
if(PutTV){
  # Put time value
  ylim1 = c(1:2); ylim1[1] = MinValuey2; ylim1[2] = MaxValuey2
  if(FixXRangePTV){
    xlim1[1] <- MinXRangePTV
    xlim1[2] <- MaxXRangePTV
  }
  if(FixYRangePTV){
    ylim1[1] <- MinYRangePTV
    ylim1[2] <- MaxYRangePTV
  }
  yTitle = "Value/Stock Price"
  xTitle = "Strike Price/StockPrice"
  mTitle = "Put Time Value As Percentage of Stock Price"
  plot(xP1, yPTV, type = "b", main = mTitle,
    sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
    ylim = ylim1, pch = 1, cex = 0.5)
}


