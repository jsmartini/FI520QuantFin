# UST Book Spreads Over CMT Test.R
# rmarkdown::render("UST Book Spreads Over CMT Test.R", "word_document")
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
# Libraries
#  date - functions for handling dates
#  optimx - general purpose optimization
#  openxlsx - manipulate spreadsheet files
Packages <- c("date", "optimx", "openxlsx", "lubridate") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Fixed Parameters
#
inputFrequency <- 2
inputPar <- 1000000.0
RoughMaturity <- 5 # Years: Used to select one bond to analyze
NFactors <- 4 # Number of factors including Level, 8 or less
NBaseCurve <- 30 # Potential observation for every year for 30 years
# Plot range information
FixRange <- FALSE # For plots
FRMax <- 3.1 # Plot bounds if fixed
FRMin <- 2.4
# Input files for U. S. Treasury bond and CMT rates
# USTFileName <- 'UST20190917.xlsx'
# CMTFileName <- 'CMT20190917.xlsx' # Should have same date as UST
# mTitle = "UST: February 17, 2019" # Date in graph title
USTFileName <- 'UST20220127.xlsx'
CMTFileName <- 'CMT20220127S.xlsx' # Should have same date as UST
mTitle = "UST: January 27, 2022" # Date in graph title
# Downloaded UST data stored with date appended: use for settlement
SettlementDateMonth = 1 # 1     # Based on file name
SettlementDateDay = 27 + 2 # 24 + 4  # Current practice is + 2 days settlement
SettlementDateYear = 2022 # 2020   
source("UST Book Inputs.R") # Access UST book
sapply(UST, class) # Note: JMaturityDate will not display here with just as.date
source("UST Functions.R") # UST functions (semi-annual only)
# Select one bond for analysis
LengthUST <- length(UST$JMaturityDate) # Number of observations
z <- FALSE # Indicator switch, set to TRUE once bond is selected
JTodaysDate <- as.integer(mdy.date(SettlementDateMonth, SettlementDateDay, 
  SettlementDateYear))
for(i in 1:LengthUST){ # Find first bond in excess of RoughMaturity
  TDate <- as.integer(UST$JMaturityDate[i])
  TYears <- (as.numeric(TDate - JTodaysDate))/365.25
  if(z == FALSE && TYears > RoughMaturity){
    SelectedBond <- i
    z <- TRUE
  }
}
UST[SelectedBond,] # Console: Selected bond parameters
inputCouponRate <- UST$COUPON[SelectedBond]
inputYieldToMaturity <- UST$ASKED.YIELD[SelectedBond]
inputBondPrice <- (UST$APrice[SelectedBond]/100.0)*inputPar
MaturityDateMonth <- month(as.date(UST$JMaturityDate[SelectedBond]))
MaturityDateDay <- day(as.date(UST$JMaturityDate[SelectedBond])) 
MaturityDateYear <- year(as.date(UST$JMaturityDate[SelectedBond]))
BONDInputData <- list(inputFrequency, inputCouponRate, inputPar, 
  inputYieldToMaturity, inputBondPrice, 
  SettlementDateMonth, SettlementDateDay, SettlementDateYear, 
  MaturityDateMonth, MaturityDateDay, MaturityDateYear)
names(BONDInputData) <- c("Frequency", "CouponRate", "Par", 
  "YieldToMaturity", "BondPrice", 
  "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
  "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear")
# Calendar manipulations
N = CouponsRemaining(BONDInputData)
# ElapsedOutput contains fraction, JLastDate, JNextDate, and JCurrentDate
ElapsedOutput = Elapsed(BONDInputData)
# Number of Total Days
NTD <- ElapsedOutput$NextDate - ElapsedOutput$LastDate
# Number of Accrued Days since last semi-annual coupon
NAD <- ElapsedOutput$Fraction * NTD
# Fraction of coupon period that has elapsed already
f <- ElapsedOutput$Fraction
# Bond maturity, in years
Mat <- TimeToMaturity(BONDInputData)
NAD; NTD; f; N; Mat
# Bond value given yield to maturity
MarketQuotedBondPrice <- inputBondPrice
MarketValueOfBond <- BondValue(BONDInputData)
AccruedInterestAmount <- AccruedInterest(BONDInputData)
ModelQuotedBondPrice <- MarketValueOfBond - AccruedInterestAmount
MarketValueOfBond; AccruedInterestAmount; 
ModelQuotedBondPrice; MarketQuotedBondPrice
# Yield to maturity given bond value
# Dollars:Quoted price w/o accrued interest
inputBondPrice = MarketQuotedBondPrice 
BONDInputData$BondPrice <- inputBondPrice
EstYieldToMaturity = YieldToMaturitySolver(BONDInputData)
EstYieldToMaturity; inputYieldToMaturity
#
# Build zero coupon, annualized, cont. compounded, discount rate curve
#  Inputs: NFactors - Number of factors
#  NBaseCurve - Number of CMTs. MarketCMTRates=int. vector 1 to NBaseCurve
# CMT curve information
#
# Access UST book
source("CMT Inputs.R")
# UST functions (semi-annual only)
source("CMT Functions.R")
BONDInputData <- list(inputCMTFrequency, inputCMTCouponRate, inputCMTPar,
  inputCMTYieldToMaturity, inputCMTBondPrice, 
  CMTSettlementDateMonth, CMTSettlementDateDay, CMTSettlementDateYear, 
  CMTMaturityDateMonth, CMTMaturityDateDay, CMTMaturityDateYear)
names(BONDInputData) <- c("Frequency", "CouponRate", "Par",
  "YieldToMaturity", "BondPrice", 
  "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
  "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear")
Tau[1] <- TimeToMaturity(BONDInputData) # Set first tau to be maturity
# x filled with initial guesses
x <- numeric(NFactors)   # b (Level, slope, and curvatures)
if(NTau < 2)Sc <- numeric(1)
if(NTau >= 2) Sc <- numeric(NTau) # Scalars
for(i in 1:NFactors){
  if(i==1){
    x[1] <- MarketCMTRates[30]    # Level: Might be NA
    if(is.na(x[1]))x[1] <- 5.0     # Thus, set to 5%
    Sc[1] <- 0.0
  }
  if(i==2){
    x[2] <- MarketCMTRates[1] - MarketCMTRates[30] # Slope
    if(is.na(x[2]))x[2] <- 0.0     # Defaults to zero
    Sc[1] <- Tau[1]
  }
  if(i>2){
    x[i] <- 0
    Sc[i-2] <- Tau[i-2]
  }
}
# Given coefficients for discount curve based on LSC, 
#  estimate sum squared difference
Answer <- DiffCMTRates(x, NFactors, Sc, NBaseCurve, MarketCMTRates)
Answer
# optimx R package provides minimization routine to select LSC coefficients 
# to minimize squared differences #, all.methods=TRUE (uses all methods)
OptOutput <- optimx(par=x, fn=DiffCMTRates, NFac = NFactors, S = Sc, 
  NCMTs = NBaseCurve, MSR = MarketCMTRates, 
  method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# If 'nlminb' failed, then try a few more optimization routines, 
#  quit when first one produces answer
Counter = 0
while(is.na(OptOutput$p1)){
  Counter = Counter + 1
  if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
    S = Sc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
    method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
  if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
    S = Sc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
    method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
  if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
    S = Sc, NCMTs = NBaseCurve, MSR = MarketCMTRates, 
    method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
}
# is.data.frame(OptOutput) # yes, it is
# x <- attr(OptOutput, "details")
OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
y <- 0
y <- numeric(NFactors)
for(i in 1:NFactors){
  if(i==1)y[1] <- OptOutput$p1[1]
  if(i==2)y[2] <- OptOutput$p2[1]
  if(i==3)y[3] <- OptOutput$p3[1]
  if(i==4)y[4] <- OptOutput$p4[1]
  if(i==5)y[5] <- OptOutput$p5[1]
  if(i==6)y[6] <- OptOutput$p6[1]
  if(i==7)y[7] <- OptOutput$p7[1]
  if(i==8)y[8] <- OptOutput$p8[1]
}
y
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted input rates
SREstimates <- CMTRates(y, NFactors, Sc, NBaseCurve)
SREstimates
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted discount rates
DREstimates <- DiscountRates(y, NFactors, Sc, NBaseCurve)
DREstimates
Maturity <- seq(1:NBaseCurve) # Maturity vector for plotting
#
# Plots: Range of y axis in input file
#
# Plot footers
NFs = paste0('LSC Factors = ', NFactors)
Scs = paste0(', Scalars = ')
if(NFactors==1){
  Scs = paste0('')
  Ts = paste0('')
}
Tau1R = round(Tau[1],2)
if(NFactors==1)Ts = paste0("None")
if(NFactors==2)Ts = paste0(Tau1R)
if(NFactors==3)Ts = paste0(Tau1R)
if(NFactors==4)Ts = paste0(Tau1R, ', ', Tau[2])
if(NFactors==5)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3])
if(NFactors==6)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4])
if(NFactors==7)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4],
  ', ', Tau[5])
if(NFactors==8)Ts = paste0(Tau1R, ', ', Tau[2], ', ', Tau[3], ', ', Tau[4], 
  ', ', Tau[5], ', ', Tau[6])
sTitle = paste(NFs, Scs, Ts)
MaxValue = max(Maturity, na.rm=TRUE)
MinValue = min(0.0, Maturity, na.rm=TRUE)
xlim1 = c(1:2)
xlim1[1] = MinValue
xlim1[2] = MaxValue
if(FixRange){
  MaxValue = FRMax
  MinValue = FRMin
} else {
  MaxValue = max(MarketCMTRates, SREstimates, DREstimates, na.rm=TRUE)
  MinValue = min(MarketCMTRates, SREstimates, DREstimates, na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("Market","LSC Fit","Discount Rates")
xTitle = "Maturity"
yTitle = "Rates"
lTitle = "Variable"
plot(Maturity, MarketCMTRates, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, SREstimates, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
lines(Maturity, DREstimates, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 3, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1,1), lty = c(2,2,2), 
  col = c("black","black","black"), pch = c(1,2,3), bty = "n", title = lTitle)
#
# Assessment of CMT with selected bond
#
BONDInputData
TestCouponRemaining = CouponsRemaining(BONDInputData)
TestElapsed = Elapsed(BONDInputData)
TestElapsed <- TestElapsed$Fraction
TestAccruedInterest = AccruedInterest(BONDInputData)
TestCouponRemaining; TestElapsed; TestAccruedInterest
Maturity <- TimeToMaturity(BONDInputData)
NumberOfFactors <- NFactors
Intercept <- 0
Slope <- 0
Curvature1 <- 0
Curvature2 <- 0
Curvature3 <- 0
Curvature4 <- 0
Curvature5 <- 0
Curvature6 <- 0
Intercept <- y[1]
if(NumberOfFactors>1)Slope <- y[2]
if(NumberOfFactors>2)Curvature1 <- y[3]
if(NumberOfFactors>3)Curvature2 <- y[4]
if(NumberOfFactors>4)Curvature3 <- y[5]
if(NumberOfFactors>5)Curvature4 <- y[6]
if(NumberOfFactors>6)Curvature5 <- y[7]
if(NumberOfFactors>7)Curvature6 <- y[8]
Tau1 <- 0
Tau2 <- 0
Tau3 <- 0
Tau4 <- 0
Tau5 <- 0
Tau6 <- 0
if(NumberOfFactors>1)Tau1 <- Tau[1]
if(NumberOfFactors>3)Tau2 <- Tau[2]
if(NumberOfFactors>4)Tau3 <- Tau[3]
if(NumberOfFactors>5)Tau4 <- Tau[4]
if(NumberOfFactors>6)Tau5 <- Tau[5]
if(NumberOfFactors>7)Tau6 <- Tau[6]
LSC <- list(Maturity, NumberOfFactors, Intercept, Slope, 
  Curvature1, Curvature2, Curvature3, Curvature4, Curvature5, Curvature6,
  Tau1, Tau2, Tau3, Tau4, Tau5, Tau6)
names(LSC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope",
  "Curvature1", "Curvature2", "Curvature3", "Curvature4", "Curvature5", 
  "Curvature6", "Tau1", "Tau2", "Tau3", "Tau4", "Tau5", "Tau6")
# Analysis of bond differential
TestBondValue = BondValue(BONDInputData)
TestBondValueDF = BondValueDF(BONDInputData, LSC)
RelativeBVError = log(TestBondValueDF/TestBondValue)*100
AbsoluteBVError = TestBondValueDF - TestBondValue
TestBondValue; TestBondValueDF; RelativeBVError; AbsoluteBVError
# Analysis of yield differential
BONDInputData$BondPrice = TestBondValueDF
TestYieldToMaturityDF = YieldToMaturitySolver(BONDInputData)
YieldDiffBPs <- (inputYieldToMaturity - TestYieldToMaturityDF)*100
inputYieldToMaturity; TestYieldToMaturityDF; YieldDiffBPs; Maturity
#
# Run on UST bond set: Align CMT w bond quotes
#
LengthUST <- length(UST$JMaturityDate)
TestBondValue <- numeric(LengthUST) 
TestBondValueDF <- numeric(LengthUST)
RelativeBVError <- numeric(LengthUST)
AbsoluteBVError <- numeric(LengthUST)
TestYieldToMaturityDF <- numeric(LengthUST)
YieldDiffBPs <- numeric(LengthUST)
Maturity <- numeric(LengthUST) 
for(i in 1:LengthUST){
  BONDInputData$CouponRate <- UST$COUPON[i]
  BONDInputData$YieldToMaturity <- UST$ASKED.YIELD[i]
  BONDInputData$BondPrice <- UST$APrice[i]
  BONDInputData$MaturityDateMonth <- month(as.date(UST$JMaturityDate[i]))
  BONDInputData$MaturityDateDay <- day(as.date(UST$JMaturityDate[i])) 
  BONDInputData$MaturityDateYear <- year(as.date(UST$JMaturityDate[i]))
  TestBondValue[i] = BondValue(BONDInputData)
  Maturity[i] <- TimeToMaturity(BONDInputData)
  LSC$Maturity <- Maturity[i] #Actually not needed (Elapsed Time function)
  TestBondValueDF[i] = BondValueDF(BONDInputData, LSC)
  RelativeBVError[i] = log(TestBondValueDF[i]/TestBondValue[i])*100
  AbsoluteBVError[i] = ((TestBondValueDF[i] - TestBondValue[i])/inputPar)*100
# With or without accrued interest (?)  
  BONDInputData$BondPrice = TestBondValueDF[i] - AccruedInterest(BONDInputData)
  TestYieldToMaturityDF[i] = YieldToMaturitySolver(BONDInputData)
  YieldDiffBPs[i] <- (BONDInputData$YieldToMaturity - 
    TestYieldToMaturityDF[i])*100
}
# Relative error plot
MaxYValue = max(RelativeBVError, na.rm=TRUE)
MinYValue = min(RelativeBVError, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
xTitle = "Maturity"
yTitle = "Relative Bond Value Error"
plot(Maturity, RelativeBVError, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Absolute error plot
MaxYValue = max(AbsoluteBVError, na.rm=TRUE)
MinYValue = min(AbsoluteBVError, na.rm=TRUE)
ylim1 = c(1:2); ylim1[1] = MinYValue; ylim1[2] = MaxYValue
xTitle = "Maturity"
yTitle = "Absolute Bond Value Error"
plot(Maturity, AbsoluteBVError, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
# Yield differential plot
MaxYValue = max(YieldDiffBPs, na.rm=TRUE)
MinYValue = min(YieldDiffBPs, na.rm=TRUE)
ylim1 = c(1:2); 
# ylim1[1] = -10; ylim1[2] = 10
ylim1[1] = MinYValue; ylim1[2] = MaxYValue
xTitle = "Maturity"
yTitle = "Yield Differential in Basis Points"
plot(Maturity, YieldDiffBPs, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)

# Just years 10 to 30 (last two on-the-runs)
# Yield differential plot
# ylim1[1] = -20; ylim1[2] = 20
ylim1[1] = min(YieldDiffBPs)
ylim1[2] = max(YieldDiffBPs)
# xlim1[1] = 10; xlim1[2] = 30
xTitle = "Maturity"
yTitle = "Yield Differential in Basis Points"
plot(Maturity, YieldDiffBPs, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
# Yield
# ylim1[1] = 1.6; ylim1[2] = 2.3
ylim1[1] = min(TestYieldToMaturityDF)
ylim1[2] = max(TestYieldToMaturityDF)
# xlim1[1] = 10; xlim1[2] = 30
xTitle = "Maturity"
yTitle = "Yield to Maturity"
legtxt = c("Implied LSC-Based","Actual")
plot(Maturity, TestYieldToMaturityDF, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1,
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, UST$ASKED.YIELD, type = "b", col ="black", xlim = xlim1,
  ylim = ylim1, pch = 2, cex = 0.5)
legend("bottomright", legtxt, cex = 0.75, lwd = c(1,1), lty = c(2,2),
  col = c("black","black","black"), pch = c(1,2), bty = "n", title = lTitle)

# Relative error plot
# RelativeBVError[i] = log(TestBondValueDF[i]/TestBondValue[i])*100
# Thus in percent
xlim1[1] <- 1.0
xlim1[2] <- 10.0
MaxYValue = max(RelativeBVError, na.rm=TRUE)
MinYValue = min(RelativeBVError, na.rm=TRUE)
ylim1 = c(1:2)
ylim1[1] = -0.5
ylim1[2] = 0.5
xTitle = "Maturity"
yTitle = "Relative Bond Value Error"
plot(Maturity, RelativeBVError, type = "p", main = mTitle,
     sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
     ylim = ylim1, pch = 1, cex = 0.5)

Maturity[224:257]; RelativeBVError[224:257]

max(RelativeBVError[224:257])
min(RelativeBVError[224:257])
