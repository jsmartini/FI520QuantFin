# Bond Inputs.R
#  Contains both individual UST and CMT curve information
#  Single UST bond
# Test inputs: Xerox BB Corporate Bond
ActualBondPrice = 992890.00 # Dollars: Quoted price without accrued interest
inputFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
inputCouponRate = 3.8      # Percent
inputPar = 1000000.0        # Currency
inputYieldToMaturity = 3.970  # Percent
# Dollars: Quoted bond price without accrued interest (stubbed to -99)
inputBondPrice = -99       
SettlementDateMonth = 10     # Integer: 1-12
SettlementDateDay = 7      # Integer: 1-31
SettlementDateYear = 2019   # Integer: 1-very high number
MaturityDateMonth = 5      # Integer: 1-12
MaturityDateDay = 15        # Integer: 1-31
MaturityDateYear = 2024     # Integer: 1-very high number
#
# CMT curve information
#
NFactors <- 4 # Number of factors including Level, 8 or less
NBaseCurve <- 30 # Potential observation for every year for 30 years
#
# Input current CMT yields
#
MarketCMTRates <- numeric(NBaseCurve) # num vector size NBaseCurve, zeros
MarketCMTRates[MarketCMTRates == 0] <- NA # Replace with NAs
MarketCMTRates[1] <- 2.55
MarketCMTRates[2] <- 2.48
MarketCMTRates[3] <- 2.46
MarketCMTRates[5] <- 2.46
MarketCMTRates[7] <- 2.54
MarketCMTRates[10] <- 2.65
MarketCMTRates[20] <- 2.85
MarketCMTRates[30] <- 3.0
#
# Input current Spread  yields (BB yields, not spreads)
#
MarketSPR1Rates <- numeric(NBaseCurve) # num vector size NBaseCurve, zeros
MarketSPR1Rates[MarketSPR1Rates == 0] <- NA # Replace with NAs
MarketSPR1Rates[1] <- 3.101
MarketSPR1Rates[2] <- 3.286
MarketSPR1Rates[3] <- 3.449
MarketSPR1Rates[4] <- 3.677
MarketSPR1Rates[5] <- 3.922
MarketSPR1Rates[6] <- 4.175
MarketSPR1Rates[7] <- 4.401
MarketSPR1Rates[8] <- 4.596
MarketSPR1Rates[9] <- 4.753
MarketSPR1Rates[10] <- 4.889 
MarketSPR1Rates[15] <- 5.473
MarketSPR1Rates[20] <- 5.941
MarketSPR1Rates[25] <- 6.034
MarketSPR1Rates[30] <- 6.000
# Input Scalars: Taus for CMT and Spreads (???)
NTau <- NFactors - 2   # Must be between 0 and 7 (not inclusive, integer)
Tau <- numeric(NTau)   # b (Level, slope, and curvatures)
Tau[1] <- 2.0 # Overwritten with bond maturity in test program
if(NTau>1)Tau[2] <- 0.5
if(NTau>2)Tau[3] <- 6.0
if(NTau>3)Tau[4] <- 15.0
if(NTau>4)Tau[5] <- 0.5
if(NTau>5)Tau[6] <- 4.0
