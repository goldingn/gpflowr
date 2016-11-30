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
                    
                    if ( !(is.numeric(input_dim) &
                           length(input_dim) == 1) )
                      stop ('input_dim must be a numeric scalar (the number of dimensions the kernel acts on)')
                    
                    self$input_dim <- input_dim

                    if (is.null(active_dims)) {
                      
                      # by default, count input_dim from 0
                      self$active_dims <- seq_len(input_dim)
                      
                    } else {
                      
                      if ( !(is.numeric(active_dim) &
                             is.vector(active_dim) &&
                             length(active_dims) == input_dim) )
                        stop ('active_dims must be a numeric vector with length input_dim')
                      
                      self$active_dims <- tf$constant(array(active_dims), tf$int32)
                    }
                    
                    # add autoflow to the compute methods
                    autoflow('compute_K',
                             X = tf$placeholder(tf$float64, shape(NULL, NULL)),
                             Z = tf$placeholder(tf$float64, shape(NULL, NULL)))
                    
                    autoflow('compute_K_symm',
                             X = tf$placeholder(tf$float64, shape(NULL, NULL)))
                    
                  },
                  
                  .slice = function (x) {
                    # get the required columns of x
                    
                    # if X isn't a tensorflow object, index from 1
                    dims <- self$active_dims
                    
                    if (inherits(x, 'tensorflow.builtin.object'))
                      tf_extract_columns(x, dims - 1)
                    else
                      x[, dims, drop = FALSE]
                    
                  },

                  # kernel composition
                  `+` = function (self, other)
                    Add$new(list(self, other)),

                  `*` = function (self, other)
                    Prod$new(list(self, other)),
                    
                  # with autoflow
                  compute_K = function (X, Z)
                    self$K(X, Z),
                  
                  # with autoflow
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
                    
                    .variance = NULL,
                    
                    initialize = function (input_dim,
                                           variance = 1,
                                           active_dims = NULL) {
                      
                      super$initialize(input_dim, active_dims)
                      self$variance <- Param$new(variance, transforms$positive())
                      
                      self$.parameter_names <- c(self$.parameter_names, '.variance')
                      
                      
                    },
                    
                    Kdiag = function (X)
                      tf$fill(tf$pack(list(tf$shape(X)[0])),
                              tf$squeeze(self$variance))
                    
                  ),
                  active = list(
                    variance = kernel_parameter(".variance")
                  ))

# The White kernel
White <- R6Class('White',
                 
                 inherit = Static,
                 
                 public = list(
                   
                   K = function (X, X2 = NULL) {
                     
                     if (!exists('X2') || is.null(X2)) {
                       
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
                        
                        if (!exists('X2') || is.null(X2))
                          shape <- tf$pack(list(tf$shape(X)[0], tf$shape(X)[0]))
                        else
                          shape <- tf$pack(list(tf$shape(X)[0], tf$shape(X2)[0]))
                        
                        tf$fill(shape, tf$squeeze(self$variance))
                      }
                    ))

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
                        
                        .variance = NULL,
                        
                        .lengthscales = NULL,
                        
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
                          self$variance <- Param$new(variance, transforms$positive())# constrain positive
                          
                          if (ARD) {
                            
                            if (is.null(lengthscales))
                              lengthscales <- rep(1, input_dim)
                            else
                              lengthscales <- lengthscales * rep(1, input_dim)

                          } else {
                            
                            if (is.null(lengthscales))
                              lengthscales <- 1
                            
                          }
                          
                          self$lengthscales <- Param$new(lengthscales, transforms$positive())
                          self$ARD <- ARD
                          self$.parameter_names <- c(self$.parameter_names, '.lengthscales', '.variance')
                          
                        },
                        
                        square_dist = function (X, X2) {
                          
                          X <- tf$truediv(X, self$lengthscales)
                          Xs <- tf$reduce_sum(tf$square(X), 1L)
                          
                          if (!exists('X2') || is.null(X2)) {
                            
                            return (to(-2) * tf$matmul(X, tf$transpose(X)) +
                                      tf$reshape(Xs, c(-1L, 1L)) +
                                      tf$reshape(Xs, c(1L, -1L)))
                            
                          } else {
                            
                            X2 <- tf$truediv(X2, self$lengthscales)
                            X2s <- tf$reduce_sum(tf$square(X2), 1L)
                            
                            return (to(-2) * tf$matmul(X, tf$transpose(X2)) +
                                      tf$reshape(Xs, c(-1L, 1L)) +
                                      tf$reshape(X2s, c(1L, -1L)))
                            
                          }
                        },
                        
                        euclid_dist = function (X, X2) {
                          r2 <- self$square_dist(X, X2)
                          tf$sqrt(r2 + to(1e-12))
                        },
                        
                        Kdiag = function (X)
                          tf$fill(tf$pack(list(tf$shape(X)[0])),
                                  tf$squeeze(self$variance))
                        
                      ),
                      
                      active = list(
                        variance = kernel_parameter(".variance"),
                        lengthscales = kernel_parameter(".lengthscales")
                      ))

# The radial basis function (RBF) or squared exponential kernel
RBF <- R6Class('RBF',
               
               inherit = Stationary,
               
               public = list(
                 
                 K = function (X, X2 = NULL) {
                   X <- self$.slice(X)
                   
                   if (!exists('X2') || is.null(X2))
                     X2 <- X
                   else
                     X2 <- self$.slice(X2)
                   
                   tf$mul(self$variance,
                          tf$exp(-self$square_dist(X, X2) / to(2)))
                 }
                 
               ))

# The linear kernel
Linear <- R6Class('Linear',
               
               inherit = Kern,
               
               public = list(
                 
                 .variance = NULL,
                 
                 ARD = NULL,
                 
                 initialize = function (input_dim,
                                        variance = 1,
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
                   
                   self$variance <- Param$new(variance, transforms$positive())
                   self$.parameter_names <- c(self$.parameter_names, '.variance')
                   
                 },
                 
                 K = function (X, X2 = NULL) {
                   X <- self$.slice(X)
                   X2 <- self$.slice(X2)
                   
                   if (!exists('X2') || is.null(X2))
                     tf$matmul(tf$mul(X, self$variance), tf$transpose(X))
                   else 
                     tf$matmul(tf$mul(X, self$variance), tf$transpose(X2))
                 },
                 
                 Kdiag = function (X)
                   tf$reduce_sum(tf$square(X) * self$variance, 1L)
                 
               ),
               active = list(
                 variance = kernel_parameter(".variance")
               ))

# The Exponential kernel
Exponential <- R6Class('Exponential',
                       
                       inherit = Stationary,
                       
                       public = list(
                         
                         K = function (X, X2 = NULL) {
                           X <- self$.slice(X)
                           X2 <- self$.slice(X2)
                           r <- self$euclid_dist(X, X2)
                           self$variance * tf$exp(to(-0.5) * r)
                         }
                         
                       ))

# The Matern 1/2 kernel
Matern12 <- R6Class('Matern12',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        X <- self$.slice(X)
                        X2 <- self$.slice(X2)
                        r <- self$euclid_dist(X, X2)
                        self$variance * tf$exp(-r)
                      }
                      
                    ))

# The Matern 3/2 kernel
Matern32 <- R6Class('Matern32',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        X <- self$.slice(X)
                        X2 <- self$.slice(X2)
                        r <- self$euclid_dist(X, X2)
                        self$variance * (to(1 + sqrt(3)) * r) * tf$exp(to(-sqrt(3)) * r)
                      }
                      
                    ))

# The Matern 5/2 kernel
Matern52 <- R6Class('Matern52',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        X <- self$.slice(X)
                        X2 <- self$.slice(X2)
                        r <- self$euclid_dist(X, X2)
                        self$variance * (t(1 + sqrt(5)) * r * to(5 / 3) *
                                           tf$square(r)) *
                          tf$exp(to(-sqrt(5)) * r)
                      }
                      
                    ))

# The Cosine kernel
Cosine <- R6Class('Cosine',
                    
                    inherit = Stationary,
                    
                    public = list(
                      
                      K = function (X, X2 = NULL) {
                        X <- self$.slice(X)
                        X2 <- self$.slice(X2)
                        r <- self$euclid_dist(X, X2)
                        self$variance * tf$cos(r)
                      }
                      
                    ))

PeriodicKernel <- R6Class('PeriodicKernel',
                  # The periodic kernel. Defined in  Equation (47) of
                  # D.J.C.MacKay. Introduction to Gaussian processes. In C.M.Bishop, editor,
                  # Neural Networks and Machine Learning, pages 133--165. Springer, 1998.
                  # Derived using the mapping u=(cos(x), sin(x)) on the inputs.
                  
                  inherit = Kern,
                  
                  public = list(
                    
                    .period = NULL,
                    
                    .variance = NULL,
                    
                    .lengthscales = NULL,
                    
                    ARD = NULL,
                    
                    initialize = function (input_dim,
                                           period = 1,
                                           variance = 1,
                                           lengthscales = 1,
                                           active_dims = NULL) {
                      
                      # No ARD support for lengthscale or period yet
                      super$initialize(input_dim, active_dims)
                      
                      self$variance <- Param$new(variance, transforms$positive())
                      self$lengthscales <- Param$new(lengthscales, transforms$positive())
                      self$period <- Param$new(period, transforms$positive())
                      
                      self$.parameter_names <- c(self$.parameter_names, '.period', '.variance', '.lengthscales')
                      
                      self$ARD <- FALSE
                      
                    },
                    
                    Kdiag = function (X)
                      tf$fill(tf$pack(list(tf$shape(X)[0])), tf$squeeze(self$variance)),
                    
                    K = function (X, X2 = NULL) {
                      
                      X <- self$.slice(X)
                      X2 <- self$.slice(X2)
                      if (!exists('X2') || is.null(X2))
                        X2 <- X
                      
                      # Introduce dummy dimension so we can use broadcasting
                      f <- tf$expand_dims(X, 1L)  # now N x 1 x D
                      f2 <- tf$expand_dims(X2, 0L)  # now 1 x M x D
                      
                      r <- to(pi) * (f - f2) / self$period
                      r = tf$reduce_sum(tf$square(tf$sin(r) / self$lengthscales), 2L)
                      
                      self$variance * tf$exp(to(-0.5) * r)
                      
                    }
                    
                  ),
                  active = list(
                    period = kernel_parameter(".period"),
                    variance = kernel_parameter(".variance"),
                    lengthscales = kernel_parameter(".lengthscales")
                  ))

make_kernel_names <- function (kern_list) {
  # Take a list of kernels and return a list of strings, giving each kernel a
  # unique name.
  # Each name is made from the lower-case version of the kernel's class name.
  # Duplicate kernels are given training numbers.
  n <- length(kern_list)
  names <- rep(NA, n) 
  counting <- list()
  
  for (k in seq_len(n)) {
    
    kernel <- kern_list[[k]]
    raw_name <- tolower(class(kernel)[1])
    
    # check for duplicates: start numbering if needed
    if (raw_name %in% names(counting)) {
      
      # if there's already been one, go back and make the first one x_1
      if (counting[[raw_name]] == 1) {
        which_first <- match(raw_name, names)
        names[which_first] <- paste0(raw_name, '_1')
      }
      
      # add one ot the counter and create the name
      counting[[raw_name]] <- counting[[raw_name]] + 1
      name = sprintf('%s_%s',
                     raw_name,
                     counting[[raw_name]])
      
      } else {
        counting[[raw_name]] <- 1
        name <- raw_name
      }
    
      names[k] <- name
    
  }
  
  names
  
}

Combination <- R6Class('Combination',
                       inherit = Kern,
                       public = list(
                         
                         kern_list = NULL,
                         
                         initialize = function (kern_list) {
                           
                           is_kernel <- vapply(kern_list, inherits, TRUE, 'Kern')
                           if (!all(is_kernel))
                             stop ('can only combine kernels')
                           
                           # initialize at maximum dimension
                           dims <- vapply(kern_list, function (x) x$input_dim, 1)
                           super$initialize(input_dim = max(dims))
                           
                           self$kern_list <- list()
                           
                           for (k in kern_list) {
                             if (inherits(k, class(self)[1]))
                               self$kern_list <- c(self$kern_list, k$kern_list)
                             else
                               self$kern_list <- c(self$kern_list, k)
                           }
                           
                           # add names to list
                           names <- make_kernel_names(self$kern_list)
                           names(self$kern_list) <- names
                           
                         }
                       ))

# additive kernel
Add <- R6Class('Add',
               inherit = Combination,
               public = list(
                 
                 K = function (X, X2 = NULL) {
                   
                   Ks <- lapply(self$kern_list, function (x) x$K(X, X2))
                   names(Ks) <- NULL
                   tf$add_n(Ks)
                 
                 },
                 
                 Kdiag = function (X) {
                   
                   Ks <- lapply(self$kern_list, function (x) x$Kdiag(X))
                   names(Ks) <- NULL
                   tf$add_n(Ks)
                   
                 }
                 
               ))

Prod <- R6Class('Prod',
               inherit = Combination,
               public = list(
                 
                 K = function (X, X2 = NULL) {
                   
                   Ks <- lapply(self$kern_list, function (x) x$K(X, X2))
                   tf_mul_n(Ks)
                   
                 },
                 
                 Kdiag = function (X) {
                   
                   Ks <- lapply(self$kern_list, function (x) x$Kdiag(X))
                   tf_mul_n(Ks)
                   
                 }
                 
               ))

#' @name kernels
#'   
#' @title GPflow kernel objects
#'   
#' @description Methods to construct, combine and evaluate kernels (covariance functions)
#'   
#' @section Usage: \preformatted{
#'  # static kernel objects
#'  k <- kernels$White(input_dim, variance = 1, active_dims = NULL)
#'  k <- kernels$Constant(input_dim, variance = 1, active_dims = NULL)
#'  k <- kernels$Bias(input_dim, variance = 1, active_dims = NULL)
#'  
#'  # stationary kernel objects
#'  k <- kernels$RBF(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  k <- kernels$Exponential(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  k <- kernels$Matern12(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  k <- kernels$Matern32(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  k <- kernels$Matern52(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  
#'  # non-stationary kernel objects
#'  k <- kernels$Linear(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  
#'  # periodic kernel objects
#'  k <- kernels$Cosine(input_dim, variance = 1, lengthscales = NULL, active_dims = NULL, ARD = FALSE)
#'  k <- kernels$PeriodicKernel(input_dim, period = 1, variance = 1, lengthscales = NULL, active_dims = NULL)
#'   
#'  # kernel operations
#'  k + k
#'  k * k
#'  
#'  # kernel object member functions
#'  k$K(X, X2 = NULL)
#'  k$Kdiag(X)
#'  k$compute_K(X, Z)
#'  k$compute_K_symm(X)
#'  #k$compute_Kdiag(X)
#' }
#'  
#' @section Arguments:
#' \describe{

#' \item{input_dim}{An integer vector giving the dimensions of the matrix 
#' \code{X} on which member functions will operate.} \item{active_dims}{An
#' integer vector identifying the columns of \code{X} on which this kernel
#' operates. If \code{NULL} (default), all columns are active} \item{variance}{A
#' positive numeric scalar giving the initial value of the marginal variance of
#' the kernel.} \item{lengthscales}{A positive numeric vector giving the initial
#' value of kernel lengthscales for the columns indexed by \code{active_dims}.} 
#' \item{period}{A positive numeric vector giving the initial value of the 
#' periodicity for the columns indexed by \code{active_dims}.} 
#' \item{ARD}{Whether allow lengthscales and periodicities to vary between
#' active dimensions. If \code{FALSE}, \code{lengthscales} and \code{period}
#' should be scalar.}
#' }
NULL

#' @export
#' @include module-class.R param.R R6-magic.R
kernels <- module(White,
                  Constant,
                  Bias = Constant,
                  RBF,
                  Linear,
                  Exponential,
                  Matern12,
                  Matern32,
                  Matern52,
                  Cosine,
                  PeriodicKernel)
