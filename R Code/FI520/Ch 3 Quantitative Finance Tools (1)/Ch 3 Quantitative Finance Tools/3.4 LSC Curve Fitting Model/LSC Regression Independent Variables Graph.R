
Maturity <- seq(1,NumberOfMaturities,1)
Maturity[1] <- 0.00001
for(i in 2:NumberOfMaturities)Maturity[i] <- i - 1
Factors <- matrix(nrow = NumberOfFactors, ncol = NumberOfMaturities)
for (j in 1:NumberOfFactors) {
  for (i in 1:NumberOfMaturities) {
    if (j == 1) Factors[j,i] <- 1.0
    else if (j == 2) Factors[j,i] = (1.0 - exp(-Maturity[i]/Scalars[j-1])) / 
        (Maturity[i]/Scalars[j-1])
    else Factors[j, i] = (1.0 - exp(-Maturity[i]/Scalars[j-2])) / 
        (Maturity[i]/Scalars[j-2]) - exp(-Maturity[i]/Scalars[j-2])
  }
}
# Plots
x <- Maturity
y1 <- Factors[1,]        # Helps to check output
if(NumberOfFactors > 1) y2 <- Factors[2,]
if(NumberOfFactors > 2) y3 <- Factors[3,]
if(NumberOfFactors > 3) y4 <- Factors[4,]
if(NumberOfFactors > 4) y5 <- Factors[5,]
MinXValue = 0; MaxXValue = max(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
# MinYValue = min(y1, y2,na.rm = TRUE); MaxYValue = max(y1, y2,na.rm = TRUE)
MinYValue = 0.0; MaxYValue = 1.0
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
xTitle = "Maturity in Years"
yTitle = "Regression Independent Variables"
lTitle <- "Parameter"
if(NumberOfFactors == 1){
  plot(x, y1, type="b", xlab=xTitle, 
    ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
  legtxt = c("Level")
  legend("right", legtxt, cex = 0.75, lwd = c(1), lty = c(1),
    col = c("black"), pch = c(1), bty = "n", title = lTitle)
} else if(NumberOfFactors == 2){
  sTitle = paste0("Scalar(1) = ", Scalars[1])
  plot(x, y1, type="b", sub=sTitle, xlab=xTitle, 
    ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
  lines(Maturity, y2, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)
  legtxt = c("Level","Slope")
  legend("right", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 1),
    col = c("black","black"), pch = c(1, 2), bty = "n", title = lTitle)
} else if(NumberOfFactors == 3){ 
  sTitle = paste0("Scalar(1) = ", Scalars[1])
  plot(x, y1, type="b", sub=sTitle, xlab=xTitle, 
    ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
  lines(Maturity, y2, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)
  lines(Maturity, y3, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 3, cex = 0.5)
  legtxt = c("Level","Slope", "Curvature1")
  legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1), lty = c(1, 1, 1),
    col = c("black","black","black"), pch = c(1, 2, 3), bty = "n", 
    title = lTitle)
} else if(NumberOfFactors == 4){ 
  sTitle = paste0("Scalar(1) = ", Scalars[1], ", Scalar(2) = ", Scalars[2])
  plot(x, y1, type="b", sub=sTitle, xlab=xTitle, 
    ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
  lines(Maturity, y2, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)
  lines(Maturity, y3, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 3, cex = 0.5)
  lines(Maturity, y4, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 4, cex = 0.5)
  legtxt = c("Level","Slope", "Curvature1", "Curvature2")
  legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1), lty = c(1, 1, 1, 1),
    col = c("black","black","black","black"), pch = c(1,2,3,4), bty = "n", 
    title = lTitle)
} else if(NumberOfFactors == 5){ 
  sTitle = paste0("Scalar(1) = ", Scalars[1], ", Scalar(2) = ", Scalars[2],
    ", Scalar(3) = ", Scalars[3])
  plot(x, y1, type="b", sub=sTitle, xlab=xTitle, 
    ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
  lines(Maturity, y2, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)
  lines(Maturity, y3, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 3, cex = 0.5)
  lines(Maturity, y4, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 4, cex = 0.5)
  lines(Maturity, y5, type="b", col="black", xlim = xlim1, 
    ylim = ylim1, pch = 5, cex = 0.5)
  legtxt = c("Level","Slope", "Curvature1", "Curvature2", "Curvature3")
  legend("right", legtxt, cex = 0.75, lwd = c(1, 1, 1, 1, 1), 
    lty = c(1, 1, 1, 1, 1), col = c("black","black","black","black","black"), 
    pch = c(1,2,3,4,5), bty = "n", title = lTitle)
}

