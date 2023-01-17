# 1A.1 Introduction to R Appendix A.R
# rmarkdown::render("1A.1 Introduction to R Appendix A.R", "word_document")
# Taken directly from the Appendix A in _An Introduction to R_ (R Core Team)
# Some additional clarifying comments have been added
# See manuals referenced from the following command line, place cursor on line 7 
# after removing the comment symbol, #, and click square before the "Run" text
# help.start()
# Illustration 1: Two standard normal random vectors
x <- rnorm(50) # Generates random normal vector of size 50
y <- rnorm(x) # Generates second random normal vector (as with x, size 50)
# Generate plot of two pseudo-random normal vectors of x- and y-coordinates
plot(x, y)
# Plot the points in the plane. A graphics window will appear automatically
ls() # List Objects: See which R objects are now in the R workspace.
rm(x, y) # Remove objects no longer needed. (Clean up).
rm(list=ls()) # Remove everything
# Illustration 2: Linear models
x <- 1:20 # Make x = (1; 2; ... ; 20) of type integer
w <- 1 + sqrt(x)/2 # A simple transformation of x
w
dummy <- data.frame(x=x, y = x + rnorm(x)*w) # Create a data frame with x and y
# There are now two x, dummy$x and x
dummy # Look at data frame
fm <- lm(y ~ x, data=dummy) # Run simple linear regression
summary(fm) # Send summary information to Console
# Fit a simple linear regression and look at the analysis
# With y to the left of the tilde, we are modelling y dependent on x
# Since we know the transformation, we can do a weighted regression
fm1 <- lm(y ~ x, data=dummy, weight=1/w^2)
summary(fm1)
rm(x)
# Illustration 3: Attaching a database
dummy$x # Vector of size 20, 1 to 20
# x # Should not exist (in markdown would cause MS Word document to fail)
# ?'attach'
attach(dummy)
dummy$x
x # Exists as x within dummy is attached
# Make the columns in the data frame visible as variables
# Illustration 4: Polynomial regression and various plots
# ?'lowess'
lrf <- lowess(x, y)
# Make a nonparametric local regression function that is nonlinear
plot(x, y) # Standard default point plot
lines(x, lrf$y) # Add in the local regression
# ?'abline': Adds straight lines through the current plot.
abline(0, 1, lty=3) # The true regression line: (intercept 0, slope 1)
abline(coef(fm)) # Unweighted regression line
abline(coef(fm1), col = "red") # Weighted regression line
detach(dummy) 
plot(fitted(fm), resid(fm), xlab="Fitted values", ylab="Residuals",
  main="Residuals vs Fitted")
# A standard regression diagnostic plot to check for heteroscedasticity
qqnorm(resid(fm), main="Residuals Rankit Plot")
# A normal scores plot to check for skewness, kurtosis and outliers
rm(fm, fm1, lrf, w, dummy)
# Clean up again.
# Illustration 5: Data analysis
# Michelson's classical experiment on the speed of light
# This dataset is provided with R
filepath <- system.file("data", "morley.tab" , package="datasets")
filepath # Get the path to the data file
# file.show(filepath) # Optional. Look at the file
mm <- read.table(filepath) # Read in the Michelson data as a data frame
mm # Note the five experiments and each has 20 runs (sl-speed of light)
mm$Expt <- factor(mm$Expt) # Change Expt and Run into factors
mm$Run <- factor(mm$Run)
mm$Expt
attach(mm)
# Compare the five experiments with simple boxplots
plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No.")
# Analyze as a randomized block, with ‘runs’ and ‘experiments’ as factors
fm <- aov(Speed ~ Run + Expt, data=mm)
summary(fm)
fm0 <- update(fm, . ~ . - Run)
# Fit the sub-model w/o ‘runs’, and compare using a formal analysis of variance
anova(fm0, fm)
detach()
rm(fm, fm0, filepath, mm)
# Illustration 6: Plots of pi, contour and image plots
x <- seq(-pi, pi, len=50) # x is a vector of 50 equally spaced values
y <- x 
# f is a square matrix, with rows and columns indexed by x and y respectively, of
# values of the function cos(y)=(1 + x2), 
f <- outer(x, y, function(x, y) cos(y)/(1 + x^2)) # Outer product applied to function 
f1 <- x %o% y # Just outer product x*y' (%o%)
f1
x; y; f; f1
oldpar <- par(no.readonly = TRUE) # Graphical parameters, list what can be changed
oldpar
par(pty="s") # Square plotting region
# Save the plotting parameters and set the plotting region to “square”
contour(x, y, f) # Contour plot
contour(x, y, f, nlevels=15, add=TRUE) 
# Make a contour map of f; add in more lines for more detail
fa <- (f-t(f))/2
# fa is the “asymmetric part” of f (t() is transpose)
contour(x, y, fa, nlevels=15) # Make a contour plot, ...
par(oldpar) # . . . and restore the old graphics parameters
image(x, y, f) # Display color image
image(x, y, fa) # Make some high density image plots
objects() # Objects specified in environment
rm(x, y, f, fa) # Clean up before moving on.
# Illustration 7: Complex arithmetic
th <- seq(-pi, pi, len=100)
z <- exp(1i*th) # 1i is used for the complex number i.
par(pty="s")
# Plotting complex arguments means plot imaginary versus real parts (circle)
plot(z, type="l")
w <- rnorm(100) + rnorm(100)*1i
# Sample points within the unit circle
w <- ifelse(Mod(w) > 1, 1/w, w)
# Map any outside the circle onto their reciprocal.
plot(w, xlim=c(-1,1), ylim=c(-1,1), pch="+",xlab="x", ylab="y")
lines(z) # All points are inside the unit circle (not uniform)
w <- sqrt(runif(100))*exp(2*pi*runif(100)*1i)
plot(w, xlim=c(-1,1), ylim=c(-1,1), pch="+", xlab="x", ylab="y")
lines(z) # Uniform distribution: More evenly spaced within the circle
rm(th, w, z, oldpar) # Take out the trash
# q() # Quit the R program
