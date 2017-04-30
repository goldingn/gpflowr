#' @name gpflow
#' @title GPflow Python Module
#'   
#' @importFrom reticulate import py_module_available
#' @export
#' 
#' @description \code{gpflow} is the imported GPflow python module. You can 
#'   access the various methods via the \code{$} operator. For example, to 
#'   access the submodule of kernel constructor functions type 
#'   \code{gpflow$kernels}.
#'   \code{gpflow_available()} tests whether the GPflow module is installed and  
#'   available to the version of Python being used by reticulate.
#'   
#' @details reticulate converts R objects to pyhton objects on the fly. It won't
#'   always guess how to do that correctly though, so here are a few gotchas to
#'   look out for:
#'   \itemize{
#'     \item{data - }{GPflow expects data vectors to be column vectors. Vectors
#'     should there be converted to one-column matrices before being passed to
#'     most functions, as in the example below.}
#'     \item{integers - }{Python (and therefore GPflow) is more stringent about 
#'     integers being integers than R is. For example, when specifying the 
#'     number of active dimensions of a kernel, you should make sure the number 
#'     passed is of type integer, not numeric. In most cases that just means
#'     switching \code{1} to \code{1L} etc.}
#'   }
#'   
#'   To tidy up your code, you can rename \code{gpflow} by assigning it to a new
#'   object (e.g. \code{gp <- gpflow}).
#'   
#' @section Documentation: The submodules and methods in \code{gpflow} are not 
#'   documented separately in this package. You can access them at 
#'   \url{http://gpflow.readthedocs.io/en/latest/}. If you're using a recent 
#'   version of the Rstudio editor, you can also get inline help and 
#'   autocompletion by hitting the tab key.
#'   
#' @examples
#' 
#' if (gpflow_available()) {
#' 
#'   # translation of the GP regression example from GPflow:
#'   # http://gpflow.readthedocs.io/en/latest/notebooks/regression.html
#'   
#'   # fake data
#'   N <- 12
#'   X <- matrix(runif(N))
#'   Y = sin(12 * X) + 0.66 * cos(25 * X) + matrix(rnorm(N)) * 0.1 + 3
#'   plot(X, Y)
#'   
#'   # set up kernel and GP
#'   k <- gpflow$kernels$Matern52(1L, lengthscales = 0.3)
#'   m <- gpflow$gpr$GPR(X, Y, kern=k)
#'   
#'   # adjust the likelihood
#'   m$likelihood$variance = 0.01
#'   
#'   # predict and plot
#'   xx <- matrix(seq(-0.1, 1.1, len = 100))
#'   tmp <- m$predict_y(xx)
#'   mean <- tmp[[1]]
#'   lines(mean ~ xx)
#'  
#' }
#' 
gpflow <- reticulate::import('GPflow', delay_load = TRUE)

#' @rdname gpflow
#' @export
gpflow_available <- function ()
  py_module_available("GPflow")