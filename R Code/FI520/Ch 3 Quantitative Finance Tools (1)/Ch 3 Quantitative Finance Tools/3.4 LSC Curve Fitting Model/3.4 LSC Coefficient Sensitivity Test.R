# 3.5 LSC Coefficient Sensitivity Test.R
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Inputs
NumberOfFactors <- 3
N <- NumberOfFactors - 2
Scalars <- c(1:N)
Scalars[1] <- 2.0
NumberOfObservations <- 5
NumberOfMaturities <- 30
Maturity <- seq(1, NumberOfMaturities, 1)
Factors <- matrix(nrow = NumberOfFactors - 1, ncol = NumberOfMaturities)
Factors # Note Factors is filled with NAs
for (j in 1:NumberOfFactors-1) {
  for (i in 1:NumberOfMaturities) {
    if (j == 1) Factors[j,i] = (1.0 - exp(-Maturity[i]/Scalars[j])) / 
        (Maturity[i]/Scalars[j])
    else Factors[j, i] = (1.0 - exp(-Maturity[i]/Scalars[j-1])) / 
      (Maturity[i]/Scalars[j-1]) - exp(-Maturity[i]/Scalars[j-1])
  }
}
Factors # Now Factors has x values for regression
#
# Analysis of slope
#
Intercept <- seq(5, 5, length.out=5)
Slope <- seq(-2, 2, 1)
Curvature <- seq(0, 0, length.out=5)
# Work on fitted data for plots
FittedRates <- matrix(nrow = NumberOfObservations, ncol = NumberOfMaturities)
for (i in 1:NumberOfObservations) { # Fitted values for each date
  for (j in 1:NumberOfMaturities){
    FittedRates[i,j] = Intercept[i] + Slope[i] * 
      ((1.0 - exp(-Maturity[j]/Scalars[1]))/(Maturity[j]/Scalars[1])) + 
      Curvature[i]*((1.0 - exp(-Maturity[j]/Scalars[1])) / 
      (Maturity[j]/Scalars[1]) - exp(-Maturity[j]/Scalars[1]))
  }
}
FittedRates
# Plots
x <- Maturity
y <- FittedRates
MinXValue = 0; MaxXValue = max(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MinYValue = min(y,na.rm = TRUE); MaxYValue = max(y,na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle <- ""
xTitle <- "Maturity"
yTitle <- "Rates"
lTitle <- "Parameter"
legtxt = c("Slope=-2","Slope=-1", "Slope=0", "Slope=+1", "Slope=+2")
sTitle <- paste0("Scalar = ", Scalars[1], ", NFactors = ", NumberOfFactors)
plot(x, y[1,], type="b", main=mTitle, sub=sTitle, xlab=xTitle, 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 1.0)
lines(Maturity, y[2,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 1.0)
lines(Maturity, y[3,], type="l", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 2.0)
lines(Maturity, y[4,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 4, cex = 1.0)
lines(Maturity, y[5,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 5, cex = 1.0)
legend("topright", legtxt, cex = 1.0, lwd = c(1,1,1,1,1), 
  lty = c(1,1,1,1,1), #col = c("black","black","black","black","black"), 
  pch = c(1,2,NA_integer_,4,5), bty = "n", title = lTitle)
#
# Analysis of curvature1 (zero slope)
#
Intercept <- seq(5, 5, length.out=5)
Slope <- seq(0, 0, length.out=5)
Curvature <- seq(-2, 2, 1) 
# Work on fitted data for plots
FittedRates <- matrix(nrow = NumberOfObservations, ncol = NumberOfMaturities)
for (i in 1:NumberOfObservations) { # Fitted values for each date
  for (j in 1:NumberOfMaturities){
    FittedRates[i,j] = Intercept[i] + Slope[i] * 
      ((1.0 - exp(-Maturity[j]/Scalars[1]))/(Maturity[j]/Scalars[1])) + 
      Curvature[i]*((1.0 - exp(-Maturity[j]/Scalars[1])) / 
      (Maturity[j]/Scalars[1]) - exp(-Maturity[j]/Scalars[1]))
  }
}
FittedRates
# Plots
x <- Maturity
y <- FittedRates
MinXValue = 0; MaxXValue = max(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MinYValue = min(y,na.rm = TRUE); MaxYValue = max(y,na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle <- ""
xTitle <- "Maturity"
yTitle <- "Rates"
lTitle <- "Parameter"
legtxt = c("Curvature1=-2","Curvature1=-1", "Curvature1=0", "Curvature1=+1", 
  "Curvature1=+2")
sTitle <- paste0("Slope = ", Slope[1], ",  Scalar = ", Scalars[1], 
  ", NFactors = ", NumberOfFactors)
plot(x, y[1,], type="b", main=mTitle, sub=sTitle, xlab=xTitle, 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 1.0)
lines(Maturity, y[2,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 1.0)
lines(Maturity, y[3,], type="l", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 2.0)
lines(Maturity, y[4,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 4, cex = 1.0)
lines(Maturity, y[5,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 5, cex = 1.0)
legend("topright", legtxt, cex = 1.0, lwd = c(1,1,1,1,1), 
  lty = c(1,1,1,1,1), #col = c("black","black","black","black","black"), 
  pch = c(1,2,NA_integer_,4,5), bty = "n", title = lTitle)
#
# Analysis of curvature1 (-2 slope)
#
Intercept <- seq(5, 5, length.out=5)
Slope <- seq(-2, -2, length.out=5)
Curvature <- seq(-2, 2, 1) 
# Work on fitted data for plots
FittedRates <- matrix(nrow = NumberOfObservations, ncol = NumberOfMaturities)
for (i in 1:NumberOfObservations) { # Fitted values for each date
  for (j in 1:NumberOfMaturities){
    FittedRates[i,j] = Intercept[i] + Slope[i] * 
      ((1.0 - exp(-Maturity[j]/Scalars[1]))/(Maturity[j]/Scalars[1])) + 
      Curvature[i]*((1.0 - exp(-Maturity[j]/Scalars[1])) / 
      (Maturity[j]/Scalars[1]) - exp(-Maturity[j]/Scalars[1]))
  }
}
FittedRates
# Plots
x <- Maturity
y <- FittedRates
MinXValue = 0; MaxXValue = max(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MinYValue = min(y,na.rm = TRUE); MaxYValue = max(y,na.rm = TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
mTitle <- ""
xTitle <- "Maturity"
yTitle <- "Rates"
lTitle <- "Parameter"
legtxt = c("Curvature1=-2","Curvature1=-1", "Curvature1=0", "Curvature1=+1", 
  "Curvature1=+2")
sTitle <- paste0("Slope = ", Slope[1], ",  Scalar = ", Scalars[1], 
  ", NFactors = ", NumberOfFactors)
plot(x, y[1,], type="b", main=mTitle, sub=sTitle, xlab=xTitle, 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 1.0)
lines(Maturity, y[2,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 1.0)
lines(Maturity, y[3,], type="l", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 2.0)
lines(Maturity, y[4,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 4, cex = 1.0)
lines(Maturity, y[5,], type="b", col="black", xlim = xlim1, 
  ylim = ylim1, pch = 5, cex = 1.0)
legend("bottomright", legtxt, cex = 1.0, lwd = c(1,1,1,1,1), 
  lty = c(1,1,1,1,1), #col = c("black","black","black","black","black"), 
  pch = c(1,2,NA_integer_,4,5), bty = "n", title = lTitle)
