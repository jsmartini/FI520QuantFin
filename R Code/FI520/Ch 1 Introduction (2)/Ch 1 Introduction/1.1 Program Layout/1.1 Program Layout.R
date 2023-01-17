# 1.1 Program Layout.R
# NOTE: If you copy the rmarkdown line below (delete # (comment line) first 
#   in the Console window, then R Studio produces a MS Word file of the 
#   results along with source code.)
# rmarkdown::render("1.1 Program Layout.R", "word_document")
# RStudio layout
#  This page is the File Window, multiple files open at one time
#  The page to the immediate right is the Environment Window,
#    with the Environment tab and History tab
#  The page below is the Console Window, give interface history
#  The page below to the right is the Information Window,
#   Files tab shows Project files (files within the Project folder),
#   Plots tab shows plots,
#   Packages show attached and some unattached packages
#   Help shows any requested help documentation
# Program Layout
# help(base) # One way to get help, very cryptic
# library(help = "base") # Control goes to the document
# help("Arithmetic") # Details on arithmetic operators
# Next line is not necessary but is the way libraries are included
library(base) 
# To load and attach add-on packages, either:
# help(stats) # Help tab now has details on the R Stats package
library(stats) # Statistics package is included
# Or:
require(Rcpp) # Rcpp package is included
?"Rcpp" # Alternative way to have details on the R Rcpp package
# R does not have a formal entry point like C++ (e.g., int main())
x = 100
x <- 0
# When the line above is run, you should see Values x 100 in the 
#   Environment window.
# Read: Element number 1 in vector x is equal to 100
x
# Display value of x in Console. Note that every variable is a vector, hence,
#  the output is: [1] 100 
# and x = 100 remains in the Console window
# The next line closes R (if uncommented)
# quit()
