# kernel classes

# The basic kernel class. Handles input_dim and active dims, and provides a
# generic '_slice' function to implement them.
Kern <- R6Class("Kern",
                
                inherit = Parameterized,
                
                public = list(
                  
                  input_dim = NULL,
                  
                  active_dims = NULL,
                  
                  initialize = function(input_dim, active_dims = NULL) {
                    
                    # input dim is an integer
                    # active dims is a (slice | iterable of integers | None)
                    
                    super$initialize()
                    self$input_dim <- input_dim

                    if (is.null(active_dims)) {
                      self$active_dims <- input_dim
                    } else {
                      self$active_dims <- tf$constant(active_dims, tf$int32)
                    }
                    
                  },
                  
                  .slice = function (X, X2) {
                    
                    if (inherits(self$active_dims, 'integer')) {
                      
                      X <- X[, self$active_dims]
                      
                      if (!is.null(X2))
                        X2 <- X2[, self$active_dims]
                      
                    } else {
                      
                      X <- tf$transpose(tf$gather(tf$transpose(X),
                                                  self$active_dims))
                      
                      if (! is.null(X2))
                        X2 <- tf$transpose(tf$gather(tf$transpose(X2),
                                                     self$active_dims))
                      
                    }                         
                    
                    list(X, X2)
                    
                  },
                  
                  # kernel composition
                  `+` = function (self, other)
                    Add(self, other),

                  `*` = function (self, other)
                    Prod(self, other),
                    
                  # add autoflow
                  compute_K = function (X, Z)
                    self$K(X, Z),
                  
                  # add autoflow
                  compute_K_symm = function (X)
                    self$K(X),
                  
                  print = function(x, ...) {
                    msg <- sprintf('%s kernel\n',
                                   class(self)[1])
                    cat(msg)
                  }
                  
                ))

# Kernels who don't depend on the value of the inputs are 'Static'. The only 
# parameter is a variance.
Static <- R6Class('Static',
                  
                  inherit = Kern,
                  
                  public = list(
                    
                    variance = NULL,
                    
                    initialize = function (input_dim,
                                           variance = 1,
                                           active_dims = NULL) {
                      
                      super$initialize(input_dim, active_dims)
                      self$variance <- Param$new(variance, transforms$positive)
                      
                      
                    },
                    
                    Kdiag = function (X)
                      tf$fill(tf$pack(list(tf$shape(X)[0])),
                              tf$squeeze(self$variance))
                    
                  ))

# The White kernel
White <- R6Class('White',
                 
                 inherit = Static,
                 
                 public = list(
                   
                   K = function (X, X2 = NULL) {
                     
                     if (is.null(X2)) {
                       
                       d <- tf$fill(tf$pack(list(tf$shape(X)[0])),
                                    tf$squeeze(self$variance))
                       
                       return (tf$diag(d))
                       
                     } else {
                       
                       shape <- tf$pack(list(tf$shape(X)[0]),
                                        tf$shape(X2)[0])
                       
                       return (tf$zeros(shape, tf$float64))
                       
                     }
                   }
                 ))

# The Constant (aka Bias) kernel
Constant <- R6Class('Constant',
                    
                    inherit = Static,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        
                        if (is.null(X2))
                          shape <- tf$pack(list(tf$shape(X)[1], tf$shape(X)[0]))
                        else
                          shape <- tf$pack(list(tf$shape(X)[1], tf$shape(X2)[0]))
                        
                        tf$fill(shape, tf$squeeze(self$variance))
                      }
                    ))

# Another name for the Constant kernel, included for convenience.
Bias <- Constant

# Base class for kernels that are stationary, that is, they only depend on
# 
# r = || x - x' ||
# 
# This class handles 'ARD' behaviour, which stands for 'Automatic Relevance
# Determination'. This means that the kernel has one lengthscale per
# dimension, otherwise the kernel is isotropic (has a single lengthscale).
Stationary <- R6Class('Stationary',
                      
                      inherit = Kern,
                      
                      public = list(
                        
                        variance = NULL,
                        
                        lengthscales = NULL,
                        
                        ARD = NULL,
                        
                        initialize = function (input_dim,
                                               variance = 1,
                                               lengthscales = NULL,
                                               active_dims = NULL,
                                               ARD = FALSE) {
                          
                          # - input_dim is the dimension of the input to the kernel
                          # - variance is the (initial) value for the variance parameter
                          # - lengthscales is the initial value for the lengthscales parameter
                          # defaults to 1.0 (ARD=False) or np.ones(input_dim) (ARD=True).
                          # - active_dims is a list of length input_dim which controls which
                          # columns of X are used.
                          # - ARD specifies whether the kernel has one lengthscale per dimension
                          # (ARD=TRUE) or a single lengthscale (ARD=False).
                          
                          super$initialize(input_dim, active_dims)
                          self$variance <- Param$new(variance, transforms$positive)# constrain positive
                          
                          if (ARD) {
                            
                            if (is.null(lengthscales))
                              lengthscales <- rep(1, input_dim)
                            else
                              lengthscales <- lengthscales * rep(1, input_dim)

                          } else {
                            
                            if (is.null(lengthscales))
                              lengthscales <- 1
                            
                          }
                          
                          self$lengthscales <- Param$new(lengthscales, transforms$positive)
                          self$ARD <- ARD
                          
                        },
                        
                        square_dist = function (X, X2) {
                          
                          X <- X / self$lengthscales # operates columnwise!
                          
                          Xs <- tf$reduce_sum(tf$square(X), 1L)
                          
                          if (is.null(X2)) {
                            
                            return (-2.0 * tf$matmul(X, tf$transpose(X)) +
                                      tf$reshape(Xs, c(-1L, 1L)) +
                                      tf$reshape(Xs, c(1L, -1L)))
                            
                          } else {
                            
                            X2 <- X2 / self$lengthscales
                            X2s <- tf$reduce_sum(tf$square(X2), 1L)
                            
                            return (-2 * tf$matmul(X, tf$transpose(X2)) +
                                      tf$reshape(Xs, c(-1L, 1L)) +
                                      tf$reshape(X2s, c(1L, -1L)))
                            
                          }
                        },
                        
                        euclid_dist = function (X, X2) {
                          r2 <- self$square_dist(X, X2)
                          tf$sqrt(r2 + 1e-12)
                        },
                        
                        Kdiag = function (X)
                          tf$fill(tf$pack(list(tf$shape(X)[0])),
                                  tf$squeeze(self$variance))
                        
                      ))

# The radial basis function (RBF) or squared exponential kernel
RBF <- R6Class('RBF',
               
               inherit = Stationary,
               
               public = list(
                 
                 K = function (X, X2 = NULL) {
                   lis <- self$.slice(X, X2)
                   tf$mul(self$variance, tf$exp(-self$square_dist(lis$X, lis$X2) / 2))
                 }
                 
               ))

# The linear kernel
Linear <- R6Class('Linear',
               
               inherit = Kern,
               
               public = list(
                 
                 initialize = function (input_dim,
                                        variance = 1,
                                        lengthscales = NULL,
                                        active_dims = NULL,
                                        ARD = FALSE) {
                   
                   # - input_dim is the dimension of the input to the kernel
                   # - variance is the (initial) value for the variance parameter(s)
                   # if ARD=True, there is one variance per input
                   # - active_dims is a list of length input_dim which controls
                   # which columns of X are used.
                   
                   super$initialize(input_dim, active_dims)
                   self$ARD <- ARD
                   
                   if (ARD)
                     variance <- rep(1, self$input_dim) * variance
                   
                   self$variance <- Param$new(variance, transforms$positive)
                   self$parameters <- list(self$variance)
                   
                 },
                 
                 K = function (X, X2 = NULL) {
                   lis <- self$.slice(X, X2)
                   
                   if (is.null(X2))
                     tf$matmul(X * self$variance, tf$transpose(X))
                   else 
                     tf$matmul(X * self$variance, tf$transpose(X2))
                 },
                 
                 Kdiag = function (X)
                   tf$reduce_sum(tf$square(X) * self$variance, 1L)
                 
               ))

# The Exponential kernel
Exponential <- R6Class('Exponential',
                       
                       inherit = Stationary,
                       
                       public = list(
                         
                         K = function (X, X2 = NULL) {
                           lis <- self$.slice(X, X2)
                           r <- self$euclid_dist(lis$X, lis$X2)
                           self$variance * tf$exp(-0.5 * r)
                         }
                         
                       ))

# The Matern 1/2 kernel
Matern12 <- R6Class('Matern12',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        lis <- self$.slice(X, X2)
                        r <- self$euclid_dist(lis$X, lis$X2)
                        self$variance * tf$exp(-r)
                      }
                      
                    ))

# The Matern 3/2 kernel
Matern32 <- R6Class('Matern32',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        lis <- self$.slice(X, X2)
                        r <- self$euclid_dist(lis$X, lis$X2)
                        self$variance * (1 + sqrt(3) * r) * tf$exp(-sqrt(3) * r)
                      }
                      
                    ))

# The Matern 5/2 kernel
Matern52 <- R6Class('Matern52',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        lis <- self$.slice(X, X2)
                        r <- self$euclid_dist(lis$X, lis$X2)
                        self$variance * (1 + sqrt(5) * r * 5 / 3 *
                                           tf$square(r)) *
                          tf$exp(-sqrt(5) * r)
                      }
                      
                    ))

# The Cosine kernel
Cosine <- R6Class('Cosine',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        lis <- self$.slice(X, X2)
                        r <- self$euclid_dist(lis$X, lis$X2)
                        self$variance * tf$cos(r)
                      }
                      
                    ))

# The Cosine kernel
PeriodicKernel <- R6Class('PeriodicKernel',
                  # The periodic kernel. Defined in  Equation (47) of
                  # D.J.C.MacKay. Introduction to Gaussian processes. In C.M.Bishop, editor,
                  # Neural Networks and Machine Learning, pages 133--165. Springer, 1998.
                  # Derived using the mapping u=(cos(x), sin(x)) on the inputs.
                  
                  inherit = Kern,
                  
                  public = list(
                    
                    initialize = function (input_dim,
                                           period = 1,
                                           variance = 1,
                                           lengthcales =1,
                                           active_dims = NULL) {
                      
                      # No ARD support for lengthscale or period yet
                      super$initialize(input_dim, active_dims)
                      
                      self$variance <- Param$new(variance, transforms$positive)
                      self$lengthscales <- Param$new(lengthscales, transforms$positive)
                      self$period <- Param$new(period, transforms$positive)
                      
                      self$ARD <- FALSE
                      
                    },
                    
                    Kdiag = function (X)
                      tf$fill(tf$pack(list(tf$shape(X)[0])), tf$squeeze(self$variance)),
                    
                    K = function (X, X2 = NULL) {
                      
                      lis <- self$.slice(X, X2)
                      if (is.null(lis$X2))
                        lis$X2<- lis$X
                      
                      # Introduce dummy dimension so we can use broadcasting
                      f <- tf$expand_dims(lis$X, 1)  # now N x 1 x D
                      f2 <- tf$expand_dims(lis$X2, 0)  # now 1 x M x D
                      
                      r <- pi * (f - f2) / self$period
                      r = tf$reduce_sum(tf$square(tf$sin(r) / self$lengthscales), 2)
                      
                      self$variance * tf$exp(-0.5 * r)
                      
                    }
                    
                  ))


#' @name kernels
#'   
#' @title GPflow kernel objects
#'   
#' @description Kernels 
#'   
#' @section Usage: \preformatted{
#' 
#'  # kernel objects
#'  kernels$Identity()
#'  transforms$Exp(lower = 1e-6)
#'  transforms$Log1pe(lower = 1e-6)
#'  transforms$Logistic(a = 0, b = 1)
#'  transforms$positive(lower = 1e-6)
#'   
#'  # transform object member functions
#'  t <- transforms$Identity()
#'  t$forward(-3)
#'  t$backward(0.5)
#'  t$tf_forward(-3)
#'  t$tf_log_jacobian(-3)
#'   }
#'  
#' @section Arguments:
#' \describe{
#' \item{lower}{The minimum value that positive transforms can take. This helps
#' stability during optimization, because aggressive optimizers can take
#' overly-long steps which lead to zero in the transformed variable, causing an
#' error.}
#' \item{a, b}{The location (\code{a}) and scale (\code{b}) of the logistic 
#' distribution used in the logistic transform.}
#' }
NULL

# 
# #' @export
# kernels <- module(White = White$new,
#                   Constant = Constant$new,
#                   Bias = Constant$new,
#                   RBF = RBF$new,
#                   Linear = Linear$new,
#                   Exponential = Exponential$new,
#                   Matern12 = Matern12$new,
#                   Matern32 = Matern32$new,
#                   Matern52 = Matern52$new,
#                   Cosine = Cosine$new,
#                   PeriodicKernel = PeriodicKernel$new)
