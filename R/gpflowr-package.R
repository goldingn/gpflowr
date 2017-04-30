#' @name gpflowr
#' @title Gaussian Processes in TensorFlow, via R
#' @docType package
#'   
#' @description \code{gpflowr} is a port of the GPflow python module to R. It 
#'   uses the reticulate package to directly import gpflow, rather than 
#'   re-writing any elements to be more R-like, so it may not be the most 
#'   convenient package for those not familiar with python. This package exists 
#'   to simplify (slightly) loading of GPflow in R, enables the use of R's 
#'   arithmetic operators for kernel construction, and slightly improves the 
#'   default printing of GPflow objects.
#'   
#'   The only exported objects in the package are the function
#'   \code{\link{gpflow_available}} which checks the installation, and the
#'   imported python module \code{\link{gpflow}}. The documentation for the
#'   module provides a simple example of fitting a GP model using this package.
NULL
