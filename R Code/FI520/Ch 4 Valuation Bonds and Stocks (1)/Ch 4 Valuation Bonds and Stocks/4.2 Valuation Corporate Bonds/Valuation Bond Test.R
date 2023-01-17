# Valuation Bond Test.R
# rmarkdown::render("Valuation Bond Test.R", "word_document"
rm(list = ls())  # Take out the Environment "trash"
cat("\014") # Clear Console, making error checking easier.
while (!is.null(dev.list()))  dev.off() # Clear old plots
par(family = 'Times New Roman') # Globally set fonts for graphs
Packages <- c("date", "optimx") 
if(length(setdiff(Packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(Packages, rownames(installed.packages())))
} # Make sure libraries are installed on this computer
lapply(Packages, library, character.only=TRUE) # Load and attach libraries
rm(Packages)
#
# Inputs for single bond, CMT rates, and spreads
#
FixRange <- FALSE
source("Bond Inputs.R")
#
# Bond functions for individual bonds
#
source("BOND Functions.R")
BONDInputData <- list(inputFrequency, inputCouponRate, inputPar, 
  inputYieldToMaturity, inputBondPrice, 
  SettlementDateMonth, SettlementDateDay, SettlementDateYear, 
  MaturityDateMonth, MaturityDateDay, MaturityDateYear)
names(BONDInputData) <- c("Frequency", "CouponRate", "Par", 
  "YieldToMaturity", "BondPrice", 
  "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
  "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear")
#
# Calendar manipulations
#
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
#
# Bond value given yield to maturity
#
MarketValueOfBond = BondValue(BONDInputData)
AccruedInterestAmount = AccruedInterest(BONDInputData)
QuotedBondPrice = MarketValueOfBond - AccruedInterestAmount
MarketValueOfBond; AccruedInterestAmount; QuotedBondPrice; ActualBondPrice
#
# Yield to maturity given bond value
#
inputBondPrice = ActualBondPrice #Dollars:Quoted price w/o accrued interest
BONDInputData$BondPrice <- inputBondPrice
EstYieldToMaturity = YieldToMaturitySolver(BONDInputData)
EstYieldToMaturity; inputYieldToMaturity
#
# Build zero coupon, annualized, cont. compounded, discount rate curve
#  Inputs: NFactors - Number of factors
#  NBaseCurve - Number of bonds. MarketCMTRates=int. vector 1 to NBaseCurve
#
# # Test inputs
# BONDInputData <- list(inputFrequency, inputCMTCouponRate, inputCMTPar,
#   inputCMTYieldToMaturity, inputCMTBondPrice, 
#   CMTSettlementDateMonth, CMTSettlementDateDay, CMTSettlementDateYear, 
#   CMTMaturityDateMonth, CMTMaturityDateDay, CMTMaturityDateYear)
# names(BONDInputData) <- c("Frequency", "CouponRate", "Par",
#   "YieldToMaturity", "BondPrice", 
#   "SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
#   "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear")
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
#
# Functions to run CMT and Spreads LSC and related analyses
#
source('SPREADS Functions.R') # Various Bond functions
# Illustgrate DiffSwRates: Given coefficients for discount curve based on LSC, 
#  estimate sum squared difference
Answer <- DiffSwRates(x, NFactors, Sc, NBaseCurve, MarketCMTRates)
Answer
# optimx R package provides minimization routine to select LSC coefficients 
# to minimize squared differences #, all.methods=TRUE (uses all methods)
#
# CMT Analysis
#
OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, S = Sc, 
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
#
# Spread 1 Analysis (BB curve, Bloomberg)
#
OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, S = Sc, 
  NCMTs = NBaseCurve, MSR = MarketSPR1Rates, 
  method=c('nlminb'), control=list(save.failures=FALSE, maxit=2500)) 
# If 'nlminb' failed, then try a few more optimization routines, 
#  quit when first one produces answer
Counter = 0
while(is.na(OptOutput$p1)){
  Counter = Counter + 1
  if(Counter == 1)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
    S = Sc, NCMTs = NBaseCurve, MSR = MarketSPR1Rates, 
    method=c('BFGS'), control=list(save.failures=FALSE, maxit=2500)) 
  if(Counter == 2)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
    S = Sc, NCMTs = NBaseCurve, MSR = MarketSPR1Rates, 
    method=c('Nelder-Mead'), control=list(save.failures=FALSE, maxit=2500))
  if(Counter == 3)OptOutput <- optimx(par=x, fn=DiffSwRates, NFac = NFactors, 
    S = Sc, NCMTs = NBaseCurve, MSR = MarketSPR1Rates, 
    method=c('L-BFGS-B'), control=list(save.failures=FALSE, maxit=2500)) 
}
# is.data.frame(OptOutput) # yes, it is
# x <- attr(OptOutput, "details")
OptMethod <- rownames(OptOutput[1]) # Method that provided answer, see Nash
ySPR1 <- 0
ySPR1 <- numeric(NFactors)
for(i in 1:NFactors){
  if(i==1)ySPR1[1] <- OptOutput$p1[1]
  if(i==2)ySPR1[2] <- OptOutput$p2[1]
  if(i==3)ySPR1[3] <- OptOutput$p3[1]
  if(i==4)ySPR1[4] <- OptOutput$p4[1]
  if(i==5)ySPR1[5] <- OptOutput$p5[1]
  if(i==6)ySPR1[6] <- OptOutput$p6[1]
  if(i==7)ySPR1[7] <- OptOutput$p7[1]
  if(i==8)ySPR1[8] <- OptOutput$p8[1]
}
ySPR1
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted input rates
SRSPR1Estimates <- CMTRates(ySPR1, NFactors, Sc, NBaseCurve)
SRSPR1Estimates
# Based on LSC parameters, y, and other inputs, NFactors, Sc, NBaseCurve, 
#  provide estimates of fitted discount rates
DRSPR1Estimates <- DiscountRates(ySPR1, NFactors, Sc, NBaseCurve)
DRSPR1Estimates
#
# Plots: Range of y axis in input file
#
Maturity <- seq(1:NBaseCurve) # Maturity vector for plotting
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
  MaxValue = max(MarketCMTRates, SREstimates, DREstimates, 
    MarketSPR1Rates, SRSPR1Estimates, DRSPR1Estimates, na.rm=TRUE)
  MinValue = min(MarketCMTRates, SREstimates, DREstimates, 
    MarketSPR1Rates, SRSPR1Estimates, DRSPR1Estimates, na.rm=TRUE)
}
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("CMT","CMT Fit","CMT DR","BB","BB Fit","BB DR")
mTitle = "UST and BB Yields"
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
lines(Maturity, MarketSPR1Rates, type = "p", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 4, cex = 0.5)
lines(Maturity, SRSPR1Estimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 5, cex = 0.5)
lines(Maturity, DRSPR1Estimates, type = "b", col ="black", xlim = xlim1, 
      ylim = ylim1, pch = 6, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1,1,1,1,1), 
  lty = c(2,2,2,2,2,2), 
  col = c("black","black","black","black","black","black"), pch = c(1,2,3,4,5,6), 
  bty = "n", title = lTitle)

MaxValue = max(DRSPR1Estimates-DREstimates, SRSPR1Estimates-SREstimates, 
  na.rm=TRUE)
MinValue = min(DRSPR1Estimates-DREstimates, SRSPR1Estimates-SREstimates, 
  na.rm=TRUE)
ylim1 = c(1:2)
ylim1[1] = MinValue
ylim1[2] = MaxValue
legtxt = c("DR Spreads","SR Spreads")
mTitle = "BB - CMT Spreads"
xTitle = "Maturity"
yTitle = "Spreads"
lTitle = "Variable"
plot(Maturity, DRSPR1Estimates-DREstimates, type = "p", main = mTitle,
  sub = sTitle, xlab = xTitle, ylab = yTitle, col = "black", xlim = xlim1, 
  ylim = ylim1, pch = 1, cex = 0.5)
lines(Maturity, SRSPR1Estimates-SREstimates, type = "b", col ="black", xlim = xlim1, 
  ylim = ylim1, pch = 2, cex = 0.5)
legend("topleft", legtxt, cex = 0.75, lwd = c(1,1), lty = c(2,2),  
  col = c("black","black"), pch = c(1,2), bty = "n", title = lTitle)


#
# LSC-based valuation
#
Maturity1 <- TimeToMaturity(BONDInputData)
NumberOfFactors <- NFactors
Intercept <- 0
Slope <- 0
Curvature1 <- 0
Curvature2 <- 0
Curvature3 <- 0
Curvature4 <- 0
Curvature5 <- 0
Curvature6 <- 0
Intercept <- ySPR1[1]
if(NumberOfFactors>1)Slope <- ySPR1[2]
if(NumberOfFactors>2)Curvature1 <- ySPR1[3]
if(NumberOfFactors>3)Curvature2 <- ySPR1[4]
if(NumberOfFactors>4)Curvature3 <- ySPR1[5]
if(NumberOfFactors>5)Curvature4 <- ySPR1[6]
if(NumberOfFactors>6)Curvature5 <- ySPR1[7]
if(NumberOfFactors>7)Curvature6 <- ySPR1[8]
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
LSC <- list(Maturity1, NumberOfFactors, Intercept, Slope, 
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
QuotedBondPrice <- (BONDInputData$BondPrice/BONDInputData$Par)*100.0
AccruedInterestAmount = AccruedInterest(BONDInputData)
QuotedBondPriceEstYTM <- ((TestBondValue - AccruedInterestAmount) / 
  BONDInputData$Par)*100.0
QuotedBondPriceEstLSC <- ((TestBondValueDF - AccruedInterestAmount) / 
  BONDInputData$Par)*100.0
# Expect YTM-based difference to be rounding error
PriceEstDiffYTM <- QuotedBondPriceEstYTM - QuotedBondPrice
# Expect LSC-based difference to include spread over/under BB curve
PriceEstDiffLSC <- QuotedBondPriceEstLSC - QuotedBondPrice
# Just express as percent of bond price
PriceEstDiffLSCPct <- ((QuotedBondPriceEstLSC - QuotedBondPrice) /
  QuotedBondPrice)*100.0
QuotedBondPrice; QuotedBondPriceEstYTM; QuotedBondPriceEstLSC  
PriceEstDiffYTM; PriceEstDiffLSC; PriceEstDiffLSCPct

