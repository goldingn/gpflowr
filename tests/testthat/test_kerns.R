context('kernel construction and evaluation')

test_that('rbf (Gaussian) kernel is equal to the reference', {

  lengthScale <- 1.4
  variance <- 2.3
  X <- matrix(rnorm(3))

  cov_mat_reference <- variance*exp(-.5*outer(c(X),c(X),'-')^2/lengthScale^2)
   
  # GPflowR
  kernel <- kernels$RBF(1,variance = variance, lengthscales = lengthScale)
  cov_mat <- tf$Session()$run(kernel$K(X))

  expect_that(cov_mat, equals(cov_mat_reference))

})

test_that('periodic kernel is equal to the reference', {

  referencePeriodicKernel <- function(X, lengthScale, signalVariance, period ){
    base <- matrix(0,nrow(X),nrow(X))
    for(i in 1:dim(X)[2]){
      base <- base + (sin(pi*outer(X[,i],X[,i],"-")/period)/lengthScale)^2
    }
    exp_dist <- exp( -0.5* base )
    return(signalVariance * exp_dist)
  }

  evalKernelError <- function(D, lengthscale, variance, period, X_data){
    cov_mat_reference <- referencePeriodicKernel(X_data, lengthscale, variance, period)
   
    # GPflowR
    kernel <- kernels$PeriodicKernel(D, period=period, variance=variance, lengthscales=lengthscale)
    cov_mat <- tf$Session()$run(kernel$K(X_data))

    expect_that(cov_mat, equals(cov_mat_reference))
  }

  ## test 1D
  D <- 1
  lengthScale <- 2
  variance <- 2.3
  period <- 2
  X_data <- matrix(rnorm(3*D),ncol=D)
  evalKernelError(D, lengthScale, variance, period, X_data)

  ## test 2D
  D <- 2
  lengthScale <- .7
  variance <- 2.3
  period <- 2
  X_data <- matrix(rnorm(3*D),ncol=D)
  evalKernelError(D, lengthScale, variance, period, X_data)
})
