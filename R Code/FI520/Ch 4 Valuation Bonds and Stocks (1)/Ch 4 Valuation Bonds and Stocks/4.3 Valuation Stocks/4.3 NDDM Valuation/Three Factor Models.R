# Three Factor Models.R

#
# Three factor, lnPVD, with intercept
#
LSC <- lm(formula = Div$lnPVD ~ Factors[1,] + Factors[2,] )
Betas <- LSC$coefficients # Make clear grabbing beta coefficients
#Level <- Betas[1]
Level <- Betas[1]
Slope <- Betas[2]
Curvature1 <- Betas[3]
PredictedlnPVD <- Level + Slope*Factors[1,] + Curvature1*Factors[2,]
FittedlnPVD <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
i = 1
for (j in 1:NumberOfMaturities){
  FittedlnPVD[i,j] = Level + Slope * Factors[1,j] + Curvature1*Factors[2,j]
}
# Plots
if(PlotlnPVD){
  MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
  xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
  MinlnPVD = min(Div$lnPVD, FittedlnPVD)
  MaxlnPVD = max(Div$lnPVD, FittedlnPVD)
  ylim1 = c(1:2); ylim1[1] = MinlnPVD; ylim1[2] = MaxlnPVD
  plot(Div$MaturityTime, Div$lnPVD, type="l", 
    main="LSC Model -- Three Factors (Gray-Fitted, Black-lnPVD)", 
    xlab="Maturity", ylab="PVD", col="black", xlim = xlim1, ylim = ylim1,
    pch = 2, cex = 0.5)
  lines(Div$MaturityTime, FittedlnPVD[1,], type="l", col="darkgray", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)
}
#
# Six factor, PVD, with intercept
#
LSC <- lm(formula = Div$PVD ~ Factors[1,] + Factors[2,])
Betas <- LSC$coefficients # Make clear grabbing beta coefficients
Level <- Betas[1]
Slope <- Betas[2]
Curvature1 <- Betas[3]
PredictedPVD <- Level + Slope*Factors[1,] + Curvature1*Factors[2,]
FittedPVD <- matrix(nrow = NumberOfDates, ncol = NumberOfMaturities)
i = 1
for (j in 1:NumberOfMaturities){
  FittedPVD[i,j] = Level + Slope * Factors[1,j] + Curvature1*Factors[2,j]
}
# Plots
if(PlotPVD){
  MinMaturity = 0; MaxMaturity = max(Div$MaturityTime)
  xlim1 = c(1:2); xlim1[1] = MinMaturity; xlim1[2] = MaxMaturity
  MinPVD = min(Div$PVD, FittedPVD); MaxPVD = max(Div$PVD, FittedPVD)
  ylim1 = c(1:2); ylim1[1] = MinPVD; ylim1[2] = MaxPVD
  plot(Div$MaturityTime, Div$PVD, type="l", 
    main="LSC Model -- Three Factors (Gray-Fitted, Black-PVD)", 
    xlab="Maturity", ylab="PVD", col="black", xlim = xlim1, ylim = ylim1,
    pch = 2, cex = 0.5)
  lines(Div$MaturityTime, FittedPVD[1,], type="l", col="darkgray", xlim = xlim1, 
    ylim = ylim1, pch = 2, cex = 0.5)
}
