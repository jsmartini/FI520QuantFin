# 3.4 Regression Test.R
# Insight: Simple regression
# stats: lm()
# read.table()
# rmarkdown::render("3.4 Regression Test.R", "word_document")
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
# Inputs contained in file
getwd() # Get Working Directory
# Read a file and create data frame
LSCData <- read.table("RegressionInputData.dat",header = TRUE)
# y is the interest rate; x1 and x2 are the slope and curvature factors
# More details in the next module
# Given that the data set is small, we examine it here
LSCData 
# Linear model
LSC <- lm(formula = LSCData$y~LSCData$x1+LSCData$x2)
summary(LSC) # Summary information from the OLS regression y = a + b1*x1 + b2*x2
anova(LSC) # Analysis of variance table
coefficients(LSC) # Regression coefficients
fitted(LSC) # Fitted values of y (y-hat)
residuals(LSC) # Errors: y - y-hat
# Save regression coefficients in vector 'Betas'
Betas <- LSC$coefficients
Betas
# Regression coefficients are the loadings on the Level, Slope, and Curvature factors
Level <- Betas[1]
Slope <- Betas[2]
Curvature <- Betas[3]
# Plot the actual and estimated term structure
x <- seq(1:9)
y1 <- LSCData$y
y2 <- fitted(LSC)
MaxYValue = max(y1, y2); MinYValue = min(y1, y2)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxXValue = max(x); MinXValue = min(x)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
legtxt = c("Actual Y","Fitted Y")
mTitle = "Illustration of Regression"
xTitle = "X"
yTitle = "Y"
lTitle = "Parameter"
plot(x, y1, type = "b", main = mTitle, xlab = xTitle, ylab = yTitle, 
  col = "black", xlim = xlim1, ylim = ylim1, pch = 1, cex = 0.5, lty = 1)
lines(x, y2, type = "b", col ="black", xlim = xlim1, ylim = ylim1, 
  pch = 2, cex = 0.5, lty = 2)
legend("topleft", legtxt, cex = 0.75, lwd = c(1, 1), lty = c(1, 2),
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)
