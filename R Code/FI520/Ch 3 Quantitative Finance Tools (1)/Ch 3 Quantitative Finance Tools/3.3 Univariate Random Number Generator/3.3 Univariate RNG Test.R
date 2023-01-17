# 3.3 Univariate RNG Test.R
# Insight: Illustrate uniform random number generation
# stats: dunif(), punif(), qunif(), mean(), sd()
# getOptions(), options(), format(), .Machine$integer.max, plot(), hist(), curve()
# Univariate random number generator examples
# rmarkdown::render("3.3 Univariate RNG Test.R", "word_document")
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
# Input parameters
NumberOfObservations = 1 # Sample size
LowerBound = 0 # Minimum
UpperBound = 1 # Maximum
# Generates a uniform random number
SampleDraw = runif(NumberOfObservations, LowerBound, UpperBound) 
QuantileValue = 0.75 # Quantiles
# Probability density function
PDF = dunif(QuantileValue, LowerBound, UpperBound, log = FALSE) 
# Cumulative distribution function
CDF = punif(QuantileValue, LowerBound, UpperBound, lower.tail = TRUE, log.p = FALSE) 
# Quantile function
QF = qunif(CDF, LowerBound, UpperBound, lower.tail = TRUE, log.p = FALSE) 
getOption("digits")
options(digits = 4) # Data for graphs less cluttered
#
# Example 1: Standard uniform numeric with range 0 to 1
#
NumberOfObservations = 100
x = c(1:NumberOfObservations)
y = c(1:NumberOfObservations)
LowerBound = 0 # Minimum
UpperBound = 1 # Maximum
y = runif(NumberOfObservations, LowerBound, UpperBound)
yMean = mean(y)
# Details on format: (converts to character)
# trim - if FALSE, values are right justified to common width, if TRUE leading 
#  blanks for justification are suppressed
# digits - "how many significant digits are to be used," not sure
# nsmall - minimum number of digits to the right of the decimal point
# justify - left, right, or centered
yMean <- format(yMean, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
yStdDev = sd(y)
yStdDev <- format(yStdDev, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
xlim1 = c(1:2)
ylim1 = c(1:2)
MinValue = min(x)
MaxValue = max(x)
xlim1[1] = MinValue
xlim1[2] = MaxValue
MinValue = min(y)
MaxValue = max(y)
ylim1[1] = MinValue
ylim1[2] = MaxValue
Title1 = "Uniform[0,1]"
yTitle = "Value"
xTitle = "Draw"
plot(x,y,type="p",main=paste0("Random Sample of ", Title1),
  sub=paste0("Mean = ", yMean, ", Standard Deviation = ", yStdDev), 
  xlab=xTitle, ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5)
yTitle = "PDF"
hist(y,main=paste0("Random Sample of ", Title1), breaks=20, freq=FALSE, col="black", 
  labels = FALSE, sub=paste0("Mean = ",yMean,", Standard Deviation = ", yStdDev), 
  xlab=xTitle,ylab=yTitle, plot=TRUE, axes=TRUE, density=10)  
curve(dunif(x, 0, 1), add=TRUE, col="black", lwd=4) 
#
# Example 2: Uniform integer with range LowerBound to UpperBound
#
LowerBound = -100L
UpperBound = 100L
NumberOfIntegers = UpperBound - LowerBound + 1
NumberOfObservations = 100
x = c(1:NumberOfObservations)
y = c(1:NumberOfObservations)
# Integer sample
.Machine$integer.max # Largest integer value on this machine
# sample.int: NumberOfIntegers sample range 1:NumberOfIntegers
# size - sample size
# replace - sample values replaced or not
# prob - vector of probability weights
y = sample.int(NumberOfIntegers, size = NumberOfObservations, replace = TRUE, prob = NULL)
y = y + LowerBound - 1 # Adjust integer vector for lower bound, lower bound not possible
yMean = mean(y)
yMean <- format(yMean, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
yStdDev = sd(y)
yStdDev <- format(yStdDev, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
xlim1 = c(1:2); ylim1 = c(1:2); MinValue = min(x); MaxValue = max(x)
xlim1[1] = MinValue; xlim1[2] = MaxValue; MinValue = min(y); MaxValue = max(y)
ylim1[1] = MinValue; ylim1[2] = MaxValue
Title1 = "Uniform[LB,UB] (Integer)"; yTitle = "Integer"; xTitle = "Draw"
plot(x,y,type="p",main=paste0("Random Sample of\n", Title1),
  sub=paste0(xTitle,"\nMean = ", yMean, ", Standard Deviation = ", yStdDev, 
     "\nNumber of Observations = ", NumberOfObservations), 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5, xlab="")
yTitle = "PDF"
hist(y,main=paste0("Random Sample of\n", Title1), breaks=10, freq=FALSE, col="black", 
  labels = FALSE, sub=paste0("Mean = ",yMean,", Standard Deviation = ", yStdDev), 
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)
curve(dunif(x, LowerBound, UpperBound), add=TRUE, col="black", lwd=4) 
#
# Example 3: Uniform numeric with range LowerBound to UpperBound
#
LowerBound = -100.0
UpperBound = 900.0
NumberOfObservations = 100
x = c(1:NumberOfObservations)
y = runif(NumberOfObservations, LowerBound, UpperBound)
yMean = mean(y)
yMean <- format(yMean, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
yStdDev = sd(y)
yStdDev <- format(yStdDev, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
xlim1 = c(1:2); ylim1 = c(1:2)
xlim1[1] = 0; xlim1[2] = NumberOfObservations
ylim1[1] = LowerBound; ylim1[2] = UpperBound
Title1 = "Uniform[LB,UB] (Numeric)"
yTitle = "Numeric"
xTitle = "Draw"
plot(x,y,type="p",main=paste0("Random Sample of\n", Title1),
  sub=paste0(xTitle,"\nMean = ", yMean, ", Standard Deviation = ", yStdDev, 
    "\nNumber of Observations = ", NumberOfObservations), 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5, xlab="")
yTitle = "PDF"
hist(y,main=paste0("Random Sample of\n", Title1), breaks=10, freq=FALSE, col="black", 
  labels = FALSE, sub=paste0("Mean = ", yMean,", Standard Deviation = ", yStdDev), 
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)
curve(dunif(x, LowerBound, UpperBound), add=TRUE, col="black", lwd=4) 
#
# Example 4: Standard normal (mean 0 and standard deviation 1)
#
NumberOfObservations = 100
x = c(1:NumberOfObservations)
y = c(1:NumberOfObservations)
PMean = 0
PStandardDeviation = 1
y = rnorm(NumberOfObservations, PMean, PStandardDeviation) 
yMean = mean(y)
yMean <- format(yMean, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
yStdDev = sd(y)
yStdDev <- format(yStdDev, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
xlim1 = c(1:2); ylim1 = c(1:2)
MinValue = min(x); MaxValue = max(x)
xlim1[1] = MinValue; xlim1[2] = MaxValue
MinValue = min(y); MaxValue = max(y)
ylim1[1] = MinValue; ylim1[2] = MaxValue
Title1 = "Normal[0,1] (Real)"
yTitle = "Value"
xTitle = "Draw"
plot(x,y,type="p",main=paste0("Random Sample of\n", Title1),
  sub=paste0(xTitle,"\nMean = ", yMean, ", Standard Deviation = ", yStdDev, 
    "\nNumber of Observations = ", NumberOfObservations), 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5, xlab="")
yTitle = "PDF"
yPDF <- 1/sqrt(2*pi) # Mode of standard normal PDF
MinValue = 0; MaxValue = yPDF
ylim1[1] = MinValue; ylim1[2] = MaxValue + 0.15
hist(y,main=paste0("Random Sample of \n", Title1), breaks=10, freq=FALSE, col="black", 
  labels = FALSE, sub=paste0("Mean = ", yMean,", Standard Deviation = ", yStdDev), 
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10, ylim = ylim1)
curve(dnorm(x, 0, 1), add=TRUE, col="black", lwd=4) 
#
# Example 5: Normal (mean and standard deviation)
#
PMean = 10.0
PStandardDeviation = 30.0
NumberOfObservations = 10000
x = c(1:NumberOfObservations)
y = c(1:NumberOfObservations)
y = rnorm(NumberOfObservations, PMean, PStandardDeviation) 
yMean = mean(y)
yMean <- format(yMean, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
yStdDev = sd(y)
yStdDev <- format(yStdDev, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
xlim1 = c(1:2); ylim1 = c(1:2)
MinValue = min(x); MaxValue = max(x)
xlim1[1] = MinValue; xlim1[2] = MaxValue
MinValue = min(y); MaxValue = max(y)
ylim1[1] = MinValue; ylim1[2] = MaxValue
Title1 = "Normal[Mean,StdDev] (Real)"
yTitle = "Real Number"
xTitle = "Draw"
plot(x,y,type="p",main=paste0("Random Sample of\n", Title1),
  sub=paste0(xTitle,"\nMean = ", yMean, ", Standard Deviation = ", yStdDev, 
    "\nNumber of Observations = ", NumberOfObservations), 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5, xlab="")
yTitle = "PDF"
yPDF <- 1/(sqrt(2*pi)*PStandardDeviation) # Mode of standard normal PDF
MinValue = 0; MaxValue = yPDF
ylim1[1] = MinValue; ylim1[2] = MaxValue + 0.01
hist(y,main=paste0("Random Sample of\n", Title1), breaks=10, freq=FALSE, col="black", 
  labels = FALSE, sub=paste0("Mean = ", yMean, ", Standard Deviation = ", yStdDev), 
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10, ylim = ylim1)
curve(dnorm(x, PMean, PStandardDeviation), add=TRUE, col="black", lwd=4) 
#
# Example 6: Likelihood (Bernoulli distribution)
#
DesiredLikelihood = 0.25 # Probability of success
NumberOfObservations = 100
LowerBound = 0 # Minimum
UpperBound = 1 # Maximum
x = c(1:NumberOfObservations)
y = c(1:NumberOfObservations)
y = runif(NumberOfObservations, LowerBound, UpperBound)
z = c(1:NumberOfObservations)
for(i in 1:NumberOfObservations){
  if(y[i] < DesiredLikelihood) y[i] = 1
  else y[i] = 0
  z[i] = DesiredLikelihood
}
yMean = mean(y)
yMean <- format(yMean, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
yStdDev = sd(y)
yStdDev <- format(yStdDev, trim = FALSE, digits = NULL, nsmall = 4, justify = "right")
xlim1 = c(1:2); ylim1 = c(1:2)
MinValue = min(x); MaxValue = max(x)
xlim1[1] = MinValue; xlim1[2] = MaxValue
MinValue = min(y); MaxValue = max(y)
ylim1[1] = MinValue; ylim1[2] = MaxValue
Title1 = "Bernoulli[DesiredLikelihood]"
yTitle = "Value"
xTitle = "Draw"
plot(x,y,type="p",main=paste0("Random Sample of\n", Title1),
  sub=paste0(xTitle,"\nMean = ", yMean, ", Standard Deviation = ", yStdDev, 
    "\nNumber of Observations = ", NumberOfObservations), 
  ylab=yTitle, col="black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5, xlab="")
yTitle = "PDF"
hist(y,main=paste0("Random Sample of \n", Title1), breaks=10, freq=FALSE, col="black", 
  labels = FALSE, sub=paste0("Mean = ",yMean,", Standard Deviation = ", yStdDev), 
  xlab=xTitle, ylab=yTitle, plot=TRUE, axes=TRUE, density=10)

