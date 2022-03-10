library(devtools)
library(roxygen2)

roxygenise("C:/Users/freueden/Documents/GitHub")
document("C:/Users/freueden/Documents/GitHub")


### Install and load the package:
devtools::install_github("freuerde/puzzle", INSTALL_opts=c("--no-multiarch"))
library(puzzle)
