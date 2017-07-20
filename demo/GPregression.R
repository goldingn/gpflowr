# generate data
N <- 12
X <- matrix(runif(N))
Y = sin(12 * X) + 0.66 * cos(25 * X) + matrix(rnorm(N)) * 0.1 + 3
plot(X, Y, ylim=c(1,5))

# set up kernel and GP
k <- gpflow$kernels$Matern52(1L, lengthscales = 0.3)
m <- gpflow$gpr$GPR(X, Y, kern=k)

# look at model parameters
print(m)

# adjust some parameters by hand
m$likelihood$variance <- 0.01  # noise variance
m$kern$lengthscales <- 0.5     # kernel lengthscale

# prediction on a grid
x <- matrix(seq(-0.1, 1.1, len = 100))
pred <- m$predict_y(x)
predicted_mean <- pred[[1]]
predicted_variance <- pred[[2]]

# plot of the model
plotGPR <- function(m, xlim){
  x <- matrix(seq(xlim[1], xlim[2], len=100))
  pred <- m$predict_y(x)
  lower <- pred[[1]] - 2*sqrt(pred[[2]])
  upper <- pred[[1]] + 2*sqrt(pred[[2]])
  plot(m$X$value, m$Y$value,
        xlim=xlim, ylim=range(m$Y$value,lower,upper),
        xlab="X", ylab="Y")
  lines(x, pred[[1]], lwd=1.5)
  lines(x, lower, lwd=.5)
  lines(x, upper, lwd=.5)
}

plotGPR(m, xlim=c(-0.1, 1.1))

# optimize the model parameters (maximum likelihood)
m$optimize()
print(m)
plotGPR(m, xlim=c(-0.1, 1.1))

# Note that prior to optimization, it is possible to fix parameter values or to constrain them to be in a given range
m$kern$variance$fixed <- TRUE
m$kern$lengthscales$transform <- gpflow$transforms$Logistic(1e-5,5)
print(m)

# Finally, it is also possible to include and estimate a mean function in the models (also known as Universal Kriging)
meanf <- gpflow$mean_functions$Linear(1,0)
m <- gpflow$gpr$GPR(X, Y, k, meanf)
m$optimize()
print(m)
plotGPR(m, xlim=c(-.6, 1.6))
