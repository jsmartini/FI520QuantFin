# Put Call Parity.R
OptionType = 2   # 1 for call, 0 for put, 2 for both
TempData <- read.xlsx(xlsxFile = FileName, sheet = 1, 
  skipEmptyRows=FALSE)
LengthTempData <- length(TempData$date)
for(i in 1:LengthTempData){
  if(TempData$cp_flag[i] == "C"){
    TempData$IntrinsicValue[i] <- max(0, TempData$MidPrice[i] - 
      TempData$StrikePrice[i])
    TempData$LowerBound[i] <- max(0, TempData$MidPrice[i] - 
      TempData$PVDiv[i] -
      TempData$StrikePrice[i]*exp(-(TempData$Rate[i]/100)*TempData$TTM[i]))
    TempData$TimeValue[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$IntrinsicValue[i])
    TempData$TimeValueLB[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$LowerBound[i])
  }
  if(TempData$cp_flag[i] == "P"){
    TempData$IntrinsicValue[i] <- max(0, TempData$StrikePrice[i] - 
      TempData$MidPrice[i])
    TempData$LowerBound[i] <- max(0, TempData$StrikePrice[i] *
      exp(-(TempData$Rate[i]/100)*TempData$TTM[i]) + TempData$PVDiv[i] - 
      TempData$MidPrice[i])
    TempData$TimeValue[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$IntrinsicValue[i])
    TempData$TimeValueLB[i] <- max(0, TempData$MidOptionPrice[i] - 
      TempData$LowerBound[i])
  }
}
CallData <- TempData[TempData$cp_flag == 'C',]
PutData <- TempData[TempData$cp_flag == 'P',]
CallData$MidCallPrice <- CallData$MidOptionPrice
CallData$BidCallPrice <- CallData$best_bid
CallData$OfferCallPrice <- CallData$best_offer
PutData$MidPutPrice <- PutData$MidOptionPrice
PutData$BidPutPrice <- PutData$best_bid
PutData$OfferPutPrice <- PutData$best_offer
CallData$CallTTM <- CallData$TTM
PutData$PutTTM <- PutData$TTM
CallData$CallRate <- CallData$Rate
PutData$PutRate <- PutData$Rate
CallData$CallPVDiv <- CallData$PVDiv
PutData$PutPVDiv <- PutData$PVDiv
CallData$CallExerciseValue <- CallData$IntrinsicValue
PutData$PutExerciseValue <- PutData$IntrinsicValue
CallData$CallLowerBound <- CallData$LowerBound
PutData$PutLowerBound <- PutData$LowerBound
CallData$CallTimeValue <- CallData$TimeValue
PutData$PutTimeValue <- PutData$TimeValue
keep <- c("StrikePrice", "MidPrice", "BidCallPrice", "MidCallPrice", 
  "OfferCallPrice", "CallTTM", "CallRate", "CallPVDiv", "CallExerciseValue", 
  "CallLowerBound", "CallTimeValue")
CallData <- CallData[, keep] 
keep <- c("StrikePrice",  "BidPutPrice", "MidPutPrice", "OfferPutPrice", 
  "PutTTM", "PutRate", "PutPVDiv", "PutExerciseValue", "PutLowerBound", 
  "PutTimeValue")
PutData <- PutData[, keep] 
OptionData <- merge.data.frame(CallData, PutData, by.x = "StrikePrice", 
  by.y = "StrikePrice")
OptionData$DF <- exp(-(OptionData$CallRate/100.0)*OptionData$CallTTM)
TC = 0.0
OptionData$PCPUB <- 1.0 + TC - (OptionData$StrikePrice*OptionData$DF - 
  OptionData$MidPutPrice)/OptionData$MidPrice
OptionData$NormalizedCallPrice <- OptionData$MidCallPrice /
  OptionData$MidPrice
OptionData$PCPLB <- 1.0 - TC - ((OptionData$CallPVDiv + 
  OptionData$StrikePrice - OptionData$MidPutPrice)/OptionData$MidPrice)
OptionData$NormalizedStrikePrice <- OptionData$StrikePrice /
  OptionData$MidPrice
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
x1 <- OptionData$NormalizedStrikePrice*100
y1 <- OptionData$PCPLB*100
y2 <- OptionData$NormalizedCallPrice*100
y3 <- OptionData$PCPUB*100
MaxValuey = max(y1, y2, y3); MinValuey = min(y1, y2, y3)
MaxValuex = max(x1); MinValuex = min(x1)
ylim1 = c(1:2); ylim1[1] = MinValuey; ylim1[2] = MaxValuey
xlim1 = c(1:2); xlim1[1] = MinValuex; xlim1[2] = MaxValuex
if(FixXRangePCP){
  xlim1[1] <- MinXRangePCP
  xlim1[2] <- MaxXRangePCP
}
if(FixYRangePCP){
  ylim1[1] <- MinYRangePCP
  ylim1[2] <- MaxYRangePCP
}
yTitle = "Normalized PCP Bounds"
xTitle = "Strike Price/StockPrice"
lTitle = "Parameter"
mTitle = "Put Call Parity Bounds"
legtxt = c("PCP Lower Bound", "Normalized Call", "PCP Upper Bound")
plot(x1, y1, type = "b", main = mTitle, sub = sTitle, xlab = xTitle,
  ylab = yTitle, col = "black", xlim = xlim1, ylim = ylim1, pch = 1, 
  cex = 0.5)
lines(x1, y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1, pch = 2, 
  cex = 0.5)
lines(x1, y3, type = "b", col ="black", xlim = xlim1, ylim = ylim1, pch = 3, 
  cex = 0.5)
legend("topright", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
  col = c("black","black", "black"), pch = c(1, 2, 3), bty = "n", 
  title = lTitle)

