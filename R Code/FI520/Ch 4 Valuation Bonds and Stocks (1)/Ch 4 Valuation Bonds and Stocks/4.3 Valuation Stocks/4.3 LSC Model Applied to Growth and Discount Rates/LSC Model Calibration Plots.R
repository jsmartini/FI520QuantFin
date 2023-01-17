# LSC Model Calibration Plots.R
#
# Need to plot G and DR over maturities
#
Maturity <- c(1:NumberOfMaturities)
N <- LengthII
FDR <- matrix(nrow = N, ncol = NumberOfMaturities)
GrowthRates <- matrix(nrow = N, ncol = NumberOfMaturities)
for (i in 1:N) { # Fitted values for each curve
  for (j in 1:NumberOfMaturities){
    GrowthRates[i,j] <- II$GLevel[i] + 
      II$GSlope[i] * ((1.0 - exp(-Maturity[j]/ScalarG[1]))/(Maturity[j]/ScalarG[1]))
    FDR[i,j] <- II$DRLevel[i] + 
      II$DRSlope[i] * ((1.0 - exp(-Maturity[j]/ScalarG[1]))/(Maturity[j]/ScalarG[1]))
  }
}
x <- Maturity
y <- GrowthRates
MinXValue = 0; MaxXValue = 30
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MinValueY = min(y,na.rm = TRUE); MaxValueY = max(y,na.rm = TRUE)
# MinValueY = 0; MaxValueY = 0.4
FixAxisY <- TRUE
MinValueY <- -20.0
MaxValueY <- 50.0
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
mTitle = "LSC Model: Growth Rates by Sectors"
xTitle = "Maturity"
yTitle = "Growth Rates (%)"
lTitle = "Sectors"
legtxt = format(II$Ind)
cpch <- seq(1,N,1)
clwd <- c(rep(1, N))
clty <- c(rep(2,N))
# plot(x, y[1,], type="b", xlab=xTitle, ylab=yTitle, col="black", 
#      xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
# for (i in 2:N) {
#   lines(Maturity, y[i,], type="b", col="black", xlim = xlim1,
#         ylim = ylim1, pch = i, cex = 0.5)
# }
# legend("topright", legtxt, cex = 0.75, lwd = clwd, lty = clty,
#        col = c("black"), pch = cpch, bty = "n", title = lTitle)
#
# Individual industries
#
x <- Maturity
y <- GrowthRates
MinXValue = 0; MaxXValue = 30
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MinValueY = min(y,na.rm = TRUE); MaxValueY = max(y,na.rm = TRUE)
# MinValueY = 0; MaxValueY = 0.4
FixAxisY <- TRUE
MinValueY <- -20.0
MaxValueY <- 50.0
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
xTitle = "Maturity"
yTitle = "Growth Rates (%)"
lTitle = "Industries"
clwd <- c(rep(1, 1))
clty <- c(rep(2,1))
for(i in 1:LengthII){
  legtxt = format(II$Ind[i])
  cpch <- seq(i,i,1)
  mTitle = paste0("LSC Model: Growth Rates of ", II$Industry[i])
  plot(x, y[i,], type="b", main = mTitle, xlab=xTitle, ylab=yTitle, col="black",
       xlim = xlim1, ylim = ylim1, pch = cpch, cex = 0.5)
  legend("topright", legtxt, cex = 0.75, lwd = clwd, lty = clty,
         col = c("black"), pch = cpch, bty = "n", title = lTitle)
}
#
# Plot forward discount rates
#
x <- Maturity
y <- FDR
MinXValue = 0; MaxXValue = 30
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MinValueY = min(y,na.rm = TRUE); MaxValueY = max(y,na.rm = TRUE)
# MinValueY = 0; MaxValueY = 0.4
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
mTitle = "LSC Model: Forward Discount Rates by Sectors"
xTitle = "Maturity"
yTitle = "Forward Discount Rates (%)"
lTitle = "Sectors"
legtxt = format(II$Ind)
cpch <- seq(1,N,1)
clwd <- c(rep(1, N))
clty <- c(rep(2,N))
# plot(x, y[1,], type="b", xlab=xTitle, ylab=yTitle, col="black", 
#      xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
# for (i in 2:N) {
#   lines(Maturity, y[i,], type="b", col="black", xlim = xlim1,
#         ylim = ylim1, pch = i, cex = 0.5)
# }
# legend("topright", legtxt, cex = 0.75, lwd = clwd, lty = clty,
#        col = c("black"), pch = cpch, bty = "n", title = lTitle)
# Individual industries
x <- Maturity
y <- FDR
MinXValue = 0; MaxXValue = 30
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MinValueY = min(y,na.rm = TRUE); MaxValueY = max(y,na.rm = TRUE)
# MinValueY = 0; MaxValueY = 0.4
FixAxisY <- TRUE
MinValueY <- -5.0
MaxValueY <- 20.0
ylim1 = c(1:2); ylim1[1] = MinValueY; ylim1[2] = MaxValueY
xTitle = "Maturity"
yTitle = "Forward Discount Rate (%)"
lTitle = "Sectors"
clwd <- c(rep(1, 1))
clty <- c(rep(2,1))
for(i in 1:LengthII){
  legtxt = format(II$Ind[i])
  cpch <- seq(i,i,1)
  mTitle = paste0("LSC Model: Forward Discount Rate of ", II$Industry[i])
  plot(x, y[i,], type="b", main = mTitle, xlab=xTitle, ylab=yTitle, col="black",
    xlim = xlim1, ylim = ylim1, pch = i, cex = 0.5)
  legend("topright", legtxt, cex = 0.75, lwd = clwd, lty = clty,
    col = c("black"), pch = cpch, bty = "n", title = lTitle)
}

