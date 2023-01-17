# 3.2 CDF and Inverse CDF.R
# Insight: Illustrate N(d) in BSMOVM as well as machine error.
# stats: rnorm(), dnorm(), pnorm(), plot()
# Variety of statistics based on the normal distribution
# rmarkdown::render("3.2 CDF and Inverse CDF Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
Packages <- c("stats") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Inputs
NumberOfObservations = 1000 # Sample size
EstimatedMean = 0 # Mean
EstimatedStandardDeviation = 1 # Standard deviation
# Generates a random numbrer
SampleDraw = rnorm(NumberOfObservations, EstimatedMean, EstimatedStandardDeviation) 
QuantileValue = 0 # Quantiles
# Probability density function
PDF = dnorm(QuantileValue, EstimatedMean, EstimatedStandardDeviation, log = FALSE) 
# Cumulative distribution function
CDF = pnorm(QuantileValue, EstimatedMean, EstimatedStandardDeviation, 
  lower.tail = TRUE, log.p = FALSE) 
# Quantile function
QF = qnorm(CDF, EstimatedMean, EstimatedStandardDeviation, 
  lower.tail = TRUE, log.p = FALSE) 
Difference = QuantileValue - QF
QuantileValue; PDF; CDF; QF
# Plots of N(D) and D to illustrate machine error and estimation error
NumberOfObservations = 201
D <- c(1:NumberOfObservations)   # Quantile input
N <- c(1:NumberOfObservations)   # CDF input
N1 <- c(1:NumberOfObservations)  # First estimate of CDF
D1 <- c(1:NumberOfObservations)  # First estimate of quantile
DE <- c(1:NumberOfObservations)  # Quantile estimation error
N2 <- c(1:NumberOfObservations)  # Second estimate of CDF
D2 <- c(1:NumberOfObservations)  # Second estimate of quantile
NE <- c(1:NumberOfObservations)  # CDF estimation error
n <- c(1:NumberOfObservations)   # Estimated PDF
EstimatedMean = 0 # Mean
EstimatedStandardDeviation = 1 # Standard deviation
for(i in 1:NumberOfObservations){
  D[i] <- -5.0 + 0.05*as.double((i-1))
  D[i] <- D[i] * EstimatedStandardDeviation + EstimatedMean
  N1[i] <- pnorm(D[i], EstimatedMean, EstimatedStandardDeviation)
  n[i] <- dnorm(D[i], EstimatedMean, EstimatedStandardDeviation)
  D1[i] <- qnorm(N1[i], EstimatedMean, EstimatedStandardDeviation)
  DE[i] <- D1[i] - D[i]
  N[i] <- -0.005 + 0.005*as.double(i)
  D2[i] <- qnorm(N[i], EstimatedMean, EstimatedStandardDeviation)
  N2[i] <- pnorm(D2[i], EstimatedMean, EstimatedStandardDeviation)
  NE[i] <- N2[i] - N[i]
}
MaxNError <- max(abs(NE), na.rm=TRUE)
MaxDError <- max(abs(DE), na.rm=TRUE)
MaxNError; MaxDError
# Standard normal probability density function plot
MaxValue = max(n, na.rm=TRUE); MinValue = min(n, na.rm=TRUE); MaxValue; MinValue
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(D, na.rm=TRUE); MinValue = min(D ,na.rm=TRUE); MaxValue; MinValue
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
xTitle = "D"; yTitle = "n"
mTitle = "Standard Normal Probability Density Function"
plot(D, n, type = "l", main = mTitle, xlab = xTitle, ylab = yTitle, xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Standard normal cumulative distribution function plot
MaxValue = max(N1, na.rm=TRUE); MinValue = min(N1, na.rm=TRUE); MaxValue; MinValue
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(D, na.rm=TRUE); MinValue = min(D ,na.rm=TRUE); MaxValue; MinValue
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
xTitle = "D"; yTitle = "N"
mTitle = "Standard Normal Cumulative Distribution Function"
plot(D, N1, type = "l", main = mTitle, xlab = xTitle, ylab = yTitle, xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Estimation error in D
MaxValue = max(DE, na.rm=TRUE); MinValue = min(DE, na.rm=TRUE); MaxValue; MinValue
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(D, na.rm=TRUE); MinValue = min(D ,na.rm=TRUE); MaxValue; MinValue
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
xTitle = "D"; yTitle = "DE"
mTitle = "Estimation Error in D"
plot(D, DE, type = "p", main = mTitle, xlab = xTitle, ylab = yTitle, xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Estimation error in N
MaxValue = max(NE, na.rm=TRUE); MinValue = min(NE, na.rm=TRUE); MaxValue; MinValue
ylim1 = c(1:2); ylim1[1] = MinValue; ylim1[2] = MaxValue
MaxValue = max(N, na.rm=TRUE); MinValue = min(N ,na.rm=TRUE); MaxValue; MinValue
xlim1 = c(1:2); xlim1[1] = MinValue; xlim1[2] = MaxValue
xTitle = "N"; yTitle = "NE"
mTitle = "Estimation Error in N"
plot(N, NE, type = "p", main = mTitle, xlab = xTitle, ylab = yTitle, xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.75)  # pch = 2 is triangle, cex is size

