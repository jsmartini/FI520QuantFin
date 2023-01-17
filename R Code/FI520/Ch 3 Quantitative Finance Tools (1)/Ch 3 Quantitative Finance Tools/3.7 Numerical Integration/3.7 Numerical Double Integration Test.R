# 3.7 Numerical Double Integration Test.R
# function(), plot(), lines(), legend(), integrate()
# Normal and lognormal statistics
# rmarkdown::render("3.7 Numerical Integration Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  stats: integrate(), single integral
#  pbivnorm: pbivnorm(), standard bivariate normal distribution
#  mvtnorm: pmvnorm(), multivariate normal CDF for arbitrary limits and rho
#  pracma: integral2(), double integral
#  plot3D: mesh()
Packages <- c("stats", "pbivnorm", "mvtnorm", "pracma", "plot3D") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
# Test Data
StockPrice = 100.0
StrikePrice = 100.0
InterestRate = 12.0
DividendYield = 0.0
Volatility = 30.0
TimeToMaturity = 1.0
source('Normal Distribution Functions.R')
source('Lognormal Distribution Functions.R')
#
# Double integral tests
#
a = 0
b = 0
rho1 = 0
# Standard bivariate normal CDF, pbivnorm(), bivariate only
N2CDFTest1 <- pbivnorm(x = a, y = b, rho = rho1, recycle = TRUE)
N2CDFTest1
# Standard bivariate normal CDF, pmvnorm(), multivariate function
mean1 <- rep(0,2)
lower1 <- rep(-Inf,2)
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- 0.0
corr1[upper.tri(corr1)] <- 0.0
upper1 <- c(0,0)
N2CDFTest2 <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
N2CDFTest2
# Single integral, univariate normal CDF
N1CDFTest1 <- integrate(NormalPDF, -Inf, 0, 0, 1)[1]
N1CDFTest1

# Double integral, two univariate normal CDF
Normal2PDF <- function(x1, x2, Mu1, Mu2, SD1, SD2, rho){
  z1 <- (x1 - Mu1) / SD1
  z2 <- (x2 - Mu2) / SD2
  z <- z1^2 -2*rho*z1*z2 + z2^2
  PDF <- ( 1.0 / (2.0*pi*SD1*SD2*sqrt(1-rho^2)) ) * 
    exp( -(z / (2.0*sqrt(1 - rho^2))) )
  return(PDF)
}

Mu1 <- 0
Mu2 <- 0
SD1 <- 1
SD2 <- 1
rho <- 0
x1min <- Mu1 - 5*SD1
x2min <- Mu2 - 5*SD2
N2CDFTest1 <- integral2(Normal2PDF, xmin = x1min, xmax = 0, ymin = x2min, 
  ymax = 0, reltol = 1e-6, Mu1 = Mu1, Mu2 = Mu2, SD1 = SD1, SD2 = SD2, 
  rho = rho)[1]
N2CDFTest1

NX1Obs <- 51
NX2Obs <- 51
Mu1 <- 0
Mu2 <- 0
SD1 <- 1
SD2 <- 1
rho <- 0
MinX1 <- Mu1 - 5*SD1
MinX2 <- Mu2 - 5*SD2
MaxX1 <- Mu1 + 5*SD1
MaxX2 <- Mu2 + 5*SD2
X1Incr <- (MaxX1 - MinX1) / (NX1Obs - 1)
X2Incr <- (MaxX2 - MinX2) / (NX2Obs - 1)
N2CDFV1 <- array(data = -99, dim = c(NX1Obs, NX2Obs)) # integral2()
mean1 <- rep(0,2)
mean1[1] <- Mu1
mean1[2] <- Mu2
lower1 <- rep(-Inf,2)
lower1[1] <- MinX1
lower1[2] <- MinX2
corr1 <- diag(2)
corr1[lower.tri(corr1)] <- rho
corr1[upper.tri(corr1)] <- rho
upper1 <- c(0,0)
N2CDFV2 <- array(data = -99, dim = c(NX1Obs, NX2Obs)) # pmvnorm()
N2CDFV3 <- array(data = -99, dim = c(NX1Obs, NX2Obs)) # pbivnorm() 
x1 <- as.numeric(array(data = NA, dim = c(NX1Obs)))
x2 <- as.numeric(array(data = NA, dim = c(NX2Obs)))
Error12 <- array(data = 0, dim = c(NX1Obs, NX2Obs)) # integral - pmvnorm
Error13 <- array(data = 0, dim = c(NX1Obs, NX2Obs)) # integral - pbivnorm
Error23 <- array(data = 0, dim = c(NX1Obs, NX2Obs)) # pmvnorm - pbivnorm
for(i in 1:NX1Obs){
  for(j in 1:NX2Obs){
    x1[i] <- MinX1 + X1Incr * i
    x2[j] <- MinX2 + X2Incr * j
    MaxX1 <- as.numeric(x1[i])
    MaxX2 <- as.numeric(x2[j])
    N2CDFV1[i, j] <- as.numeric(integral2(Normal2PDF, xmin = MinX1, xmax = MaxX1,
      ymin=MinX2, ymax = MaxX2, reltol = 1e-6, Mu1 = Mu1, Mu2 = Mu2,
      SD1 = SD1, SD2 = SD2, rho = rho)[1])
    upper1[1] <- MaxX1
    upper1[2] <- MaxX2
    N2CDFV2[i, j] <- pmvnorm(lower=lower1, upper=upper1, mean=mean1, corr=corr1)[1]
# pbivnorm(): Only STANDARD bivariate normal CDF
    N2CDFV3[i, j] <- pbivnorm(x = MaxX1, y = MaxX2, rho = rho, recycle = TRUE)
    Error12[i, j] <-  N2CDFV1[i, j] - N2CDFV2[i, j]
    Error13[i, j] <-  N2CDFV1[i, j] - N2CDFV3[i, j]
    Error23[i, j] <-  N2CDFV2[i, j] - N2CDFV3[i, j]
  }
}

M <- mesh(x1, x2)
x3 <- M$x + 0*M$y
y3 <- 0*M$x + M$y

mTitle1 <- "Bivariate CDF (integral2)" 
# phi1 <- 40
# theta1 <- 40
phi1 <- 45
theta1 <- 45
MaxXValue = max(x3)
MinXValue = min(x3)
xlim1 = c(1:2); xlim1[1] = MinXValue; xlim1[2] = MaxXValue
MaxYValue = max(y3)
MinYValue = min(y3)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
MaxZValue = max(N2CDFV1)
MinZValue = min(N2CDFV1)
zlim1 = c(1:2); zlim1[1] = MinZValue; zlim1[2] = MaxZValue
xTitle <- "X"
yTitle <- "Y"
zTitle <- "Z"
surf3D(x = x3, y = y3, z = N2CDFV1, colkey = FALSE, bty = "b2", 
  main = mTitle1, phi = phi1, theta = theta1, 
  xlim = xlim1, ylim = ylim1, zlim = zlim1,
  xlab = xTitle, ylab = yTitle, zlab = zTitle,
  axes = TRUE, scale=TRUE, box=TRUE)
surf3D(x = x3, y = y3, z = N2CDFV2, colkey = FALSE, bty = "b2", 
       main ="Bivariate CDF (pmvnorm)", col=NULL)
surf3D(x = x3, y = y3, z = N2CDFV3, colkey = FALSE, bty = "b2", 
       main ="Standard Bivariate CDF (pbivnorm)")
surf3D(x = x3, y = y3, z = Error12, colkey = TRUE, bty = "b2", 
       main ="Error (integral2 - pmvnorm)")
surf3D(x = x3, y = y3, z = Error13, colkey = TRUE, bty = "b2", 
       main ="Error (integral2 - pbivnorm)")
surf3D(x = x3, y = y3, z = Error23, colkey = TRUE, bty = "b2", 
       main ="Error (pmvnorm - pbivnorm)")

MaxError12 <- max(abs(Error12))
MaxError13 <- max(abs(Error13))
MaxError23 <- max(abs(Error23))
MaxError12; MaxError13; MaxError23
