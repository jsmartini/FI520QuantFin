# LSC Model Calibration.R
# Import as data.frame each tab of data in input spreadsheet
# II - industry input data.frame
# MI - model inputs
II <- read.xlsx(xlsxFile = InputFileName, sheet = "Industry Inputs", 
  skipEmptyRows = FALSE)
MI <- read.xlsx(xlsxFile = InputFileName, sheet = "Model Inputs", 
  skipEmptyRows = FALSE)
#
# Phase 1: Calibrate LSC model based on II and MI
#
# Core model inputs
NumberOfFactors <- MI$Value[1]
N <- pmax(1,NumberOfFactors - 2)
ScalarG <- c(1:N)
ScalarDR <- c(1:N)
for(i in 1:N){
  ScalarG[i] <- MI$Value[i+1]
  ScalarDR[i] <- MI$Value[N+i+1]
}
Lg <- MI$Value[2*N+2]
Damper <- MI$Value[2*N+3]
LengthII <- length(II$Price) # Number of industries
#
# Phase 2: Solve for implied expected cash flow growth rate slope
#
source("LSC Model Functions.R")
inputInstrumentValue <- -99
inputGLevel = Lg 
inputGSlope = -99 # Placeholer
inputGCurve1 = 0
inputDRLevel = -99 
inputDRSlope = -99 
inputDRCurve1 = 0
inputScalarG = ScalarG
inputScalarDR = ScalarDR
inputNumberOfYears <- NumberOfMaturities
inputDR <- -99 # Placeholder
inputInitialCF <- 1.0
inputImpliedLowerBound <- -10
inputImpliedUpperBound <- 10
#  Input matrix
#  LSCInputData - eventually generalize
LSCModelInputs <- list(inputGLevel, inputGSlope, inputGCurve1, 
  inputDRLevel, inputDRSlope, inputDRCurve1, 
  inputScalarG, inputScalarDR, 
  inputNumberOfYears, inputDR, inputInitialCF, 
  inputImpliedLowerBound, inputImpliedUpperBound)
names(LSCModelInputs) <- c("LSCGLevel", "LSCGSlope", "LSCGCurve1",
  "LSCDRLevel", "LSCDRSlope", "LSCDRCurve1",
  "LSCScalarG", "LSCScalarDR", 
  "LSCNumberOfYears", "LSCDR", "LSCInitialCF", 
  "ImpliedLowerBound", "ImpliedUpperBound")
II$CF <- II$Price*II$DY # Initial dollar dividend amount
II$PCF <- II$Price/II$CF
for(i in 1:LengthII){
  inputInstrumentValue <- II$Price[i]
  LSCModelInputs$LSCInitialCF <- II$CF[i]
  LSCModelInputs$LSCDR <- II$DR[i]
  # LSCModelInputs$LSCGLevel = Lg  
  II$GSlope[i] = ImpliedLSCGSlope(LSCModelInputs, inputInstrumentValue)
  LSCModelInputs$LSCGSlope <- II$GSlope[i]
  II$ILSCGValue[i] <- LSCInstrumentValueG(LSCModelInputs)
  II$GLevel[i] <- LSCModelInputs$LSCGLevel
  II$ScalarG[i] <- LSCModelInputs$LSCScalarG
  II$ScalarDR[i] <- LSCModelInputs$LSCScalarDR
}
#
# Phase 3: Now solve for implied forward DR slope
#
# Compute Lf for each industry based on damper
#
AvPCF <- mean(II$PCF)
II$PCFL <- II$PCF + Damper*(AvPCF - II$PCF)
II$DRLevel <- log(1 + 1/II$PCFL) + Lg 
for(i in 1:LengthII){
  LSCModelInputs$LSCGLevel = II$GLevel[i] 
  LSCModelInputs$LSCGSlope = II$GSlope[i] 
  LSCModelInputs$LSCDRLevel = II$DRLevel[i]
  inputInstrumentValue <- II$Price[i]
  II$DRSlope[i] = ImpliedLSCDRSlope(LSCModelInputs, inputInstrumentValue)
  LSCModelInputs$LSCDRSlope <- II$DRSlope[i]
  II$ILSCDRValue[i] <- LSCInstrumentValueGDR(LSCModelInputs)
}
# Phase 4: Export 
II$PCF <- round(II$PCF, 4)
II$DR <- round(II$DR*100, 4)
II$GSlope <- round(II$GSlope*100, 4)
II$ILSCGValue <- round(II$ILSCGValue, 4)
II$GLevel <- round(II$GLevel*100, 4)
II$DRSlope <- round(II$DRSlope*100, 4)
II$ILSCDRValue <- round(II$ILSCDRValue, 4)
II$DRLevel <- round(II$DRLevel*100, 4)
write.xlsx(II, "LSC Output File.xlsx", sheetName = "Industry Outputs")
