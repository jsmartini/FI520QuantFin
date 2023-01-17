# CMT Inputs.R
# Read CMT Yields from spreadsheet
CMT <- read.xlsx(xlsxFile = CMTFileName, sheet = 1, skipEmptyRows = FALSE)
CMT
#
# Input current CMT yields
#
MarketCMTRates <- numeric(NBaseCurve) # num vector size NBaseCurve, zeros
MarketCMTRates[MarketCMTRates == 0] <- NA # Replace with NAs
# Read in from spreadsheet
LengthCMT <- length(CMT$CMTYield)
for(j in 1:LengthCMT){
  for(i in 1:NBaseCurve){
    if(CMT$Maturity[j] == i){
      CMTMaturity <- CMT$Maturity[j]
      MarketCMTRates[CMTMaturity] <- CMT$CMTYield[j] 
    }
  }
}
# Input Scalars: Taus
NTau <- NFactors - 2   # Must be between 0 and 7 (not inclusive, integer)
Tau <- numeric(NTau)   # b (Level, slope, and curvatures)
Tau[1] <- 2.0 # Overwritten with bond maturity in test program
if(NTau>1)Tau[2] <- 3.0
if(NTau>2)Tau[3] <- 6.0
if(NTau>3)Tau[4] <- 15.0
if(NTau>4)Tau[5] <- 0.5
if(NTau>5)Tau[6] <- 4.0
# CMT test inputs
isCMTInputDifferent = FALSE
if(isCMTInputDifferent){ # Opportunity to provide other inputs
  inputCMTFrequency = 2L         # Coupon frequency per year, 1, 2, 4, or 12
  inputCMTCouponRate = 2.25      # Percent
  inputCMTPar = 1000000.0        # Currency
  inputCMTYieldToMaturity = 2.637802  # Percent
# Dollars: Quoted bond price without accrued interest  
  inputCMTBondPrice = 10000*(97 + 31.5/32) 
  CMTSettlementDateMonth = 2     # Integer: 1-12
  CMTSettlementDateDay = 12      # Integer: 1-31
  CMTSettlementDateYear = 2019   # Integer: 1-very high number
  CMTMaturityDateMonth = 11      # Integer: 1-12
  CMTMaturityDateDay = 15        # Integer: 1-31
  CMTMaturityDateYear = 2027     # Integer: 1-very high number
} else {
  inputCMTFrequency = inputFrequency
  inputCMTCouponRate = inputCouponRate
  inputCMTPar = inputPar
  inputCMTYieldToMaturity = inputYieldToMaturity
  inputCMTBondPrice = inputBondPrice
  CMTSettlementDateMonth = SettlementDateMonth
  CMTSettlementDateDay = SettlementDateDay
  CMTSettlementDateYear = SettlementDateYear
  CMTMaturityDateMonth = MaturityDateMonth
  CMTMaturityDateDay = MaturityDateDay
  CMTMaturityDateYear = MaturityDateYear
}