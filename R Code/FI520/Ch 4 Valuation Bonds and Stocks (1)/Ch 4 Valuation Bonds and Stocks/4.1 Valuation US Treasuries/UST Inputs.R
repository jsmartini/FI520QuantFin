# UST Inputs.R
#  Contains both individual UST and CMT curve information
#  Single UST bond
# Test inputs
MarketQuotedBondPrice = 975312.50 # Dollars: Quoted price without accrued interest
inputFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
inputCouponRate = 2.25      # Percent
inputPar = 1000000.0        # Currency
inputYieldToMaturity = 2.535235  # Percent
# Dollars: Quoted bond price without accrued interest (stubbed to -99)
inputBondPrice = -99       
SettlementDateMonth = 1     # Integer: 1-12
SettlementDateDay = 17      # Integer: 1-31
SettlementDateYear = 2018   # Integer: 1-very high number
MaturityDateMonth = 11      # Integer: 1-12
MaturityDateDay = 15        # Integer: 1-31
MaturityDateYear = 2027     # Integer: 1-very high number
# #
# # CMT curve information
# #
# NFactors <- 4 # Number of factors including Level, 8 or less
# NBaseCurve <- 30 # Potential observation for every year for 30 years
# #
# # Input current CMT yields
# #
# MarketCMTRates <- numeric(NBaseCurve) # num vector size NBaseCurve, zeros
# MarketCMTRates[MarketCMTRates == 0] <- NA # Replace with NAs
# MarketCMTRates[1] <- 2.55
# MarketCMTRates[2] <- 2.48
# MarketCMTRates[3] <- 2.46
# MarketCMTRates[5] <- 2.46
# MarketCMTRates[7] <- 2.54
# MarketCMTRates[10] <- 2.65
# MarketCMTRates[20] <- 2.85
# MarketCMTRates[30] <- 3.0
# # Input Scalars: Taus
# NTau <- NFactors - 2   # Must be between 0 and 7 (not inclusive, integer)
# Tau <- numeric(NTau)   # b (Level, slope, and curvatures)
# Tau[1] <- 2.0 # Overwritten with bond maturity in test program
# if(NTau>1)Tau[2] <- 3.0
# if(NTau>2)Tau[3] <- 6.0
# if(NTau>3)Tau[4] <- 15.0
# if(NTau>4)Tau[5] <- 0.5
# if(NTau>5)Tau[6] <- 4.0
# #
# # Plot range information
# #
# FixRange <- TRUE
# FRMax <- 3.1
# FRMin <- 2.4
# #
# # CMT test inputs
# #
# # Test inputs
# isCMTInputDifferent = TRUE
# if(isCMTInputDifferent){
#   inputCMTFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
#   inputCMTCouponRate = 2.25      # Percent
#   inputCMTPar = 1000000.0        # Currency
#   inputCMTYieldToMaturity = 2.637802  # Percent
# # Dollars: Quoted bond price without accrued interest  
#   inputCMTBondPrice = 10000*(97 + 31.5/32) 
#   CMTSettlementDateMonth = 2     # Integer: 1-12
#   CMTSettlementDateDay = 12      # Integer: 1-31
#   CMTSettlementDateYear = 2019   # Integer: 1-very high number
#   CMTMaturityDateMonth = 11      # Integer: 1-12
#   CMTMaturityDateDay = 15        # Integer: 1-31
#   CMTMaturityDateYear = 2027     # Integer: 1-very high number
# } else {
#   inputCMTFrequency = inputFrequency
#   inputCMTCouponRate = inputCouponRate
#   inputCMTPar = inputPar
#   inputCMTYieldToMaturity = inputYieldToMaturity
#   inputCMTBondPrice = inputBondPrice
#   CMTSettlementDateMonth = SettlementDateMonth
#   CMTSettlementDateDay = SettlementDateDay
#   CMTSettlementDateYear = SettlementDateYear
#   CMTMaturityDateMonth = MaturityDateMonth
#   CMTMaturityDateDay = MaturityDateDay
#   CMTMaturityDateYear = MaturityDateYear
# }
# 
