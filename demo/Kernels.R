# There is a large variety of kernels already implemented in gpflow
k0 <- gpflow$kernels$White(input_dim=1L)
k1 <- gpflow$kernels$Constant(input_dim=1L)
k2 <- gpflow$kernels$Linear(input_dim=1L)
k3 <- gpflow$kernels$Matern12(input_dim=1L)
k4 <- gpflow$kernels$Matern32(input_dim=1L)
k5 <- gpflow$kernels$Matern52(input_dim=1L)
k6 <- gpflow$kernels$RBF(input_dim=1L)
k7 <- gpflow$kernels$Cosine(input_dim=1L)
k8 <- gpflow$kernels$PeriodicKernel(input_dim=1L)

plot_kernel <- function(kern, main, y=0){
  x <- matrix(seq(-5, 5, len=101))
  kxy <-kern$compute_K(x,matrix(y))
  plot(x, kxy, type="l", main=main, ylab=paste0("k(x,",y,")"))
}

par(mfrow=c(2,4))
plot_kernel(k1,"Constant")
plot_kernel(k2,"Linear",y=1)
plot_kernel(k3,"Matern12")
plot_kernel(k4,"Matern32")
plot_kernel(k5,"Matern52")
plot_kernel(k6,"RBF")
plot_kernel(k7,"Cosine")
plot_kernel(k8,"PeriodicKernel")
par(mfrow=c(1,1))

# access parameter value
k4$lengthscales$value

# setting some parameter value
k8$variance <- 0.1

# Combining kernels
k9 <- k4 + k8
k10 <- k5 * k7

par(mfrow=c(1,2))
plot_kernel(k9, "kernel sum")
plot_kernel(k10,"kernel product")
par(mfrow=c(1,1))

# Kernels on multiple dimensions

k = gpflow$kernels$Matern52(input_dim=5L)  # isotropic
print(k)

k = gpflow$kernels$Matern52(input_dim=5L, ARD=TRUE)  # anisotropic
print(k)

# Active dimensions : create kernels defined on input subspaces
k1 = gpflow$kernels$Linear(1L, active_dims=list(0))
k2 = gpflow$kernels$Matern52(1L, active_dims=list(1))
k = k1 + k2

print(k)
