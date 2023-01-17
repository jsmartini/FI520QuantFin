# Generic Plots.R
x <- xT
xI <- xIT
MaxYValue <- max(yT, na.rm = TRUE)
MinYValue <- min(yT, na.rm = TRUE)
if(FixAxisY){
  MaxYValue <- MaxValueY
  MinYValue <- MinValueY
}
ylim1 <- c(1:2); ylim1[1] <- MinYValue; ylim1[2] <- MaxYValue
MaxXValue <- max(xI); MinXValue <- min(xI)
if(FixAxisX){
  MaxXValue <- MaxValueX
  MinXValue <- MinValueX
}
xlim1 <- c(1:2); xlim1[1] <- MinXValue; xlim1[2] <- MaxXValue
# Illustrate more control over plots
#  Want legend outside of box to avoid overwriting data points
par(omi = c(0.2, 0, 0, 0)) # Set outer margins at bottom to 0.2 inches
plot(xI, yT[1,], type = LineType, main = mTitle, axes = FALSE,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = LineSize)
for(i in 2:Ny){
  lines(xI, yT[i,], type = LineType, col ="black", xlim = xlim1, ylim = ylim1,
    pch = i, cex = LineSize)
}
box() # create a wrap around the points plotted
# Format x-axis
IncrementX <- round((as.numeric(MaxXValue) - as.numeric(MinXValue))/10.0,1)
TickMarksX <- c(seq(from = as.numeric(MinXValue), to = as.numeric(MaxXValue),
  by=IncrementX))
if(isxDate){
  lblX <- as.Date(TickMarksX, origin = "1960-01-01")
  lblX <- format.Date(lblX, "%b-%Y")
} else {
  lblX <- paste0(format(TickMarksX, trim = TRUE, digits = 4,
    justify = c("right"), width = 0, big.mark = ","))
}
axis(side = 1, labels = NA, tck = -0.015, at = TickMarksX)
axis(side = 1, lwd = 0, line = -0.4, at = TickMarksX, label = lblX)
# Format y-axis
IncrementY <- round((as.numeric(MaxYValue) - as.numeric(MinYValue))/10.0, 2)
TickMarksY <- c(seq(from=round(MinYValue,2),to=round(MaxYValue,2),by=IncrementY))
lblY = paste0(format(TickMarksY, trim = TRUE, digits = 4,
  justify = c("right"), width = 0, big.mark = ","))
axis(side = 2, labels = NA, tck = -0.015, at = TickMarksY)
axis(side = 2, lwd = 0, line = -0.4, las = 1, at = TickMarksY, label = lblY)
# Overlay with invisible plot solely for legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",
     sub = sTitle)
legend("bottom", legtxt, horiz = TRUE, cex = 0.7, lwd = clwd, 
  lty = clty, col = c("black"), bty = "n", pch = cpch,   
  inset = c(0, 0))
par(defaultpar) # Reset to default parameters
