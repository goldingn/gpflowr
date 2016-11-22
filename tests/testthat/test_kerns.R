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

test_that('Corregionalization kernels check', {

  #setup
  k <- kernels$Coregion(1, output_dim=3, rank=2)
  #k$W <- matrix(rnorm(6),3, 2)
  #k$kappa <- runif(3) + 1
  X <- matrix(sample(0:2,10,replace=TRUE),10,1)
  X2 <- matrix(sample(0:2,12,replace=TRUE),12,1)

  # test shape
  #K <- k$compute_K(X,X2)
  #expect_that(dim(K), equals(c(10,12)))

  #K <- k$compute_K_symm(X)
  #expect_that(dim(K), equals(c(10,10)))

  # test diag
  #K <- k$compute_K_symm(X)
  #Kdiag <- k$compute_Kdiag(X)
  #expect_that(dim(diag(K)), equals(dim(Kdiag))) # added by Nicolas
  #expect_that(diag(K), equals(Kdiag))

  # test slice
  X <- cbind(X, sample(0:2,10,replace=TRUE),10,1)
  # k1 <- kernels$Coregion(1, 3, 2, active_dims=0)
  # k2 <- kernels$RBF(1, active_dims=1)
  # k <- k1 * k2
  # K1 <- k$compute_K_symm(X)
  # K2 <- k1$compute_K_symm(X) * k2$compute_K_symm(X)  # slicing happens inside kernel
  # expect_that(K1, equals(K2))

})