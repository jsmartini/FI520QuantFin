# Test SWAP Functions.R
# Needs to be sourced from 3.5 IR Swap Valuation Test.R

# Input dataset: IRSwapBook.xlsx
FileName <- "IRSwapBook.xlsx"
SVInputs <- read.xlsx(xlsxFile = FileName, sheet = "Book1", skipEmptyRows = TRUE)
# Next line throws an error as it is expecting SVInputs$ before EM
TM <- EM
# Attach dataframe to make SVInputs variables local
attach(SVInputs)
TM <- EM
TD <- ED
TY <- EY
Convention <- FixConv # Either Modified Business Following (MBF), Modified Business Preceding (MBP)
# AdjustDate(): Attempt to fix bad dates and address MBF or MBP (week-ends)
Date1 <- AdjustDate(TM, TD, TY, Convention) # Julian
Date2 <- as.Date(Date1) # YEAR-MM-DD
Date3 <- format(Date2, "%m/%d/%y")
TM <- 1
TD <- 12
TY <- 2019
Convention1 <- 'MBP' # Saturday should convert to Friday, 01/11/19
TestDate <- format(as.Date(AdjustDate(TM, TD, TY, Convention1)), "%m/%d/%y")
TM <- 1
TD <- 12
TY <- 2019
Convention1 <- 'MBF' # Saturday should convert to Monday, 01/14/19
TestDate <- format(as.Date(AdjustDate(TM, TD, TY, Convention1)), "%m/%d/%y")


# #
# # Work on components for Fixed
# #
# NData <- list(EM, ED, EY, MM, MD, MY, Convention, FixPF)
# names(NData) <- c("SettlementDateMonth", "SettlementDateDay", "SettlementDateYear", 
#   "MaturityDateMonth", "MaturityDateDay", "MaturityDateYear", "Convention", "Frequency")
# N <- NData
# if(FixNTD == 0)NTDFix = 360
# if(FixNTD != 0)NTDFix = 365
# FixData <- PaymentsRemaining(N)
# nFix <- FixData$Counter
# CFDatesFix <- as.date(FixData$PayDates)
# # Remove NAs and sort in ascending order
# CFDatesFix <- sort(CFDatesFix[!is.na(CFDatesFix)])
# CFDatesFixDate <- format(as.Date(CFDatesFix),"%m/%d/%y")
# # CFDatesFix; CFDatesFixDate
# 
# NADFix <- as.numeric(c(1:nFix))
# NADFix[] <- 0
# APFix <- NADFix
# MaturityFix <- APFix
# for(i in 1:nFix){
#   if(FixNAD < 1) NADFix[i] = CFDatesFix[i+1] - CFDatesFix[i]
#   else {
#     if(FixPF == 1) NADFix[i] = 360
#     else if (FixPF == 2) NADFix[i] = 180
#     else if (FixPF == 4) NADFix[i] = 90
#     else NADFix[i] = 30 
#   }
#   APFix[i] = NADFix[i]/NTDFix
#   MaturityFix[i] = (CFDatesFix[i+1] - CFDatesFix[1])/365
# }
# NADFix; APFix; MaturityFix
# # Set main parameters for LSC discount rates
# Maturity = MaturityFix[1]
# NumberOfFactors = NLSCDR
# Intercept = DRParm1
# Slope = DRParm2
# Curvature1 = DRParm3
# Curvature2 = DRParm4
# Curvature3 = DRParm5
# Tau1 = DRScalar1
# Tau2 = DRScalar2
# Tau3 = DRScalar3
# Tau4 = DRScalar4
# LSC <- list(Maturity, NumberOfFactors, Intercept, Slope, Curvature1, Curvature2, Curvature3,
#   Tau1, Tau2, Tau3, Tau4)
# names(LSC) <- c("Maturity", "NumberOfFactors", "Intercept", "Slope", "Curvature1", "Curvature2",
#   "Curvature3", "Tau1", "Tau2", "Tau3", "Tau4")
# LSC
# # Calculate discount factors
# DR <- as.numeric(c(1:nFix))
# DR[] <- 0
# DF <- DR
# AFix = 0
# for(i in 1:nFix){
#   LSC$Maturity = MaturityFix[i] 
#   DR[i] = LSCRate(LSC)/100.0
#   DF[i] = exp(-DR[i]*MaturityFix[i])
#   AFix = AFix + APFix[i]*NAmt*DF[i]
# }
# DR; DF; AFix
# #
# # Work on components for Floating
# #
# if(FltNTD == 0)NTDFlt = 360
# if(FltNTD != 0)NTDFlt = 365
# N$Frequency <- FltPF
# N$Convention <- FltConv 
# FltData <- PaymentsRemaining(N)
# nFlt <- FltData$Counter
# CFDatesFlt <- as.date(FltData$PayDates)
# # Remove NAs and sort in ascending order
# CFDatesFlt <- sort(CFDatesFlt[!is.na(CFDatesFlt)])
# CFDatesFltDate <- format(as.Date(CFDatesFlt),"%m/%d/%y")
# CFDatesFlt
# CFDatesFltDate
# NADFlt <- as.numeric(c(1:nFlt))
# NADFlt[] <- 0
# APFlt <- NADFlt
# MaturityFlt <- APFlt
# for(i in 1:nFlt){
#   if(FltNAD < 1) NADFlt[i] = CFDatesFlt[i+1] - CFDatesFlt[i]
#   else {
#     if(FltPF == 1) NADFlt[i] = 360
#     else if (FltPF == 2) NADFlt[i] = 180
#     else if (FltPF == 4) NADFlt[i] = 90
#     else NADFlt[i] = 30 
#   }
#   APFlt[i] = NADFlt[i]/NTDFlt
#   MaturityFlt[i] = (CFDatesFlt[i+1] - CFDatesFlt[1])/365
# }
# NADFlt; APFlt; MaturityFlt
# # LSC discount rates for floating
# # Calculate discount factors
# DR <- as.numeric(c(1:nFlt))
# DR[] <- 0
# DF <- DR
# AFlt = 0
# for(i in 1:nFlt){
#   LSC$Maturity = MaturityFlt[i] 
#   DR[i] = LSCRate(LSC)/100.0
#   DF[i] = exp(-DR[i]*MaturityFlt[i])
# }
# DR; DF
# # LSC forward rate information
# LSC$NumberOfFactors = NLSCFR
# LSC$Intercept = FRParm1
# LSC$Slope = FRParm2
# LSC$Curvature1 = FRParm3
# LSC$Curvature2 = FRParm4
# LSC$Curvature3 = FRParm5
# LSC$Tau1 = FRScalar1
# LSC$Tau2 = FRScalar2
# LSC$Tau3 = FRScalar3
# LSC$Tau4 = FRScalar4
# FR <- as.numeric(c(1:nFlt))
# FR[] <- 0
# AFlt <- FR
# TAFlt <- 0
# # Advanced set, settled in arrears assumed
# LSC$Maturity = 0.0001 
# FR[1] = LSCRate(LSC)/100.0
# AFlt[1] = APFlt[1]*NAmt*DF[1]*FR[1]
# TAFlt = TAFlt + AFlt[1]
# for(i in 2:nFlt){
#   LSC$Maturity = MaturityFlt[i-1] 
#   FR[i] = LSCRate(LSC)/100.0
#   AFlt[i] = APFlt[i]*NAmt*DF[i]*FR[i]
#   TAFlt = TAFlt + AFlt[i]
# }
# FR; AFlt; TAFlt
# 
# FixSwapRate = (TAFlt/AFix)*100.0
# SwapValue = (FixedRate/100.0)*AFix - TAFlt
# EquilibriumSwapValueCheck = (FixSwapRate/100.0)*AFix - TAFlt
# FixSwapRate; SwapValue
# 
# detach(SVInputs)
