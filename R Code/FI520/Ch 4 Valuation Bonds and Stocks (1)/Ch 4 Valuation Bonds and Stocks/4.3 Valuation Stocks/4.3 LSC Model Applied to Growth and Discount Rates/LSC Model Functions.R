# LSC Model Functions.R
#
# Constant DR, LSC model for G
#
LSCInstrumentValueG = function(L){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    for (j in 1:LSCNumberOfYears){
      GR[j] <- LSCGLevel +
        LSCGSlope * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
          (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        PVCF[j] <- CF[j]*((1 + LSCDR)^j)
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        PVCF[j] <- CF[j]/((1 + LSCDR)^j)
      }
    }
    InstrumentValue <- sum(PVCF)
    return( InstrumentValue )
  })
}
# Implied slope function for growth
ImpliedLSCGSlope <- function(L, inputInstrumentValue){
  TestFunctionLSCImpliedGSlope<-function(testImpliedGSlope, L,
    inputInstrumentValue){
    L$LSCGSlope = testImpliedGSlope
    return( abs(inputInstrumentValue - LSCInstrumentValueG(L))^2 )
  }
# NOTE: Tolerance set very low (alternative: .Machine$double.eps^0.25)
  solution = optimize(TestFunctionLSCImpliedGSlope, L, inputInstrumentValue,
    interval = c(L$ImpliedLowerBound, L$ImpliedUpperBound),
    tol = .Machine$double.eps)
  ImpliedGSlope = solution$minimum
  L$LSCGSlope = ImpliedGSlope
  Difference = inputInstrumentValue - LSCInstrumentValueG(L)
  if (abs(Difference) < 0.01)return(ImpliedGSlope)
  else return(NA)
}
#
# LSC model for G and DR
#
LSCInstrumentValueGDR = function(L){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    DR <- c(1:LSCNumberOfYears)
    DF <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    for (j in 1:LSCNumberOfYears){
      GR[j] <- LSCGLevel +
        LSCGSlope * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
          (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      DR[j] <- LSCDRLevel +
        LSCDRSlope * ((1.0 - exp(-Mat[j]/LSCScalarDR))/(Mat[j]/LSCScalarDR)) +
        LSCDRCurve1 * ((1.0-exp(-Mat[j]/LSCScalarDR)) / 
          (Mat[j]/LSCScalarDR) - exp(-Mat[j]/LSCScalarDR))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        DF[j] <- exp(-DR[j])
        PVCF[j] <- CF[j] * DF[j]
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        DF[j] <- DF[j-1] * exp(-DR[j])
        PVCF[j] <- CF[j] * DF[j]
      }
    }
    InstrumentValue <- sum(PVCF)
    return( InstrumentValue )
  })
}
# Implied slope function for growth
ImpliedLSCDRSlope <- function(L, inputInstrumentValue){
  TestFunctionLSCImpliedDRSlope<-function(testImpliedDRSlope, L,
    inputInstrumentValue){
    L$LSCDRSlope = testImpliedDRSlope
    return( abs(inputInstrumentValue - LSCInstrumentValueGDR(L))^2 )
  }
  # NOTE: Tolerance set very low (alternative: .Machine$double.eps^0.25)
  solution = optimize(TestFunctionLSCImpliedDRSlope, L, inputInstrumentValue,
    interval = c(L$ImpliedLowerBound, L$ImpliedUpperBound),
    tol = .Machine$double.eps)
  ImpliedDRSlope = solution$minimum
  L$LSCDRSlope = ImpliedDRSlope
  Difference = inputInstrumentValue - LSCInstrumentValueGDR(L)
  if (abs(Difference) < 0.01)return(ImpliedDRSlope)
  else return(NA)
}


#
# LSC model for G and DR with adjustment for Decrement and Alpha
#
LSCInstrumentValueGDRAdj = function(L, D, A){
  with(L, {
    Mat <- c(1:LSCNumberOfYears)
    CF <- c(1:LSCNumberOfYears)
    GR <- c(1:LSCNumberOfYears)
    DR <- c(1:LSCNumberOfYears)
    DF <- c(1:LSCNumberOfYears)
    PVCF <- c(1:LSCNumberOfYears)
    for (j in 1:LSCNumberOfYears){
      GR[j] <- LSCGLevel +
        (LSCGSlope - D) * ((1.0 - exp(-Mat[j]/LSCScalarG))/(Mat[j]/LSCScalarG)) +
        LSCGCurve1 * ((1.0-exp(-Mat[j]/LSCScalarG)) / 
                        (Mat[j]/LSCScalarG) - exp(-Mat[j]/LSCScalarG))
      DR[j] <- LSCDRLevel +
        (LSCDRSlope + A) * ((1.0 - exp(-Mat[j]/LSCScalarDR))/(Mat[j]/LSCScalarDR)) +
        LSCDRCurve1 * ((1.0-exp(-Mat[j]/LSCScalarDR)) / 
                        (Mat[j]/LSCScalarDR) - exp(-Mat[j]/LSCScalarDR))
      if(j == 1){
        CF[j] <- LSCInitialCF * exp(GR[j])
        DF[j] <- exp(-DR[j])
        PVCF[j] <- CF[j] * DF[j]
      }
      if(j > 1){
        CF[j] <- CF[j-1] * exp(GR[j])
        DF[j] <- DF[j-1] * exp(-DR[j] )
        PVCF[j] <- CF[j] * DF[j]
      }
    }
    InstrumentValue <- sum(PVCF)
    return( InstrumentValue )
  })
}

