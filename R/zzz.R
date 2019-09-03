.onAttach <- function(libname, pkgname){
  msg <- "This package is NOT VALIDATED, and should be reserved for exploratory analysis only."
  packageStartupMessage(msg)
}
