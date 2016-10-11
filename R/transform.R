# transform class and descendents

# base class
Transform <- R6Class('Transform',
                     public = list(
                       
                       forward = function (x)
                         not_implemented_error(),
                       
                       backward = function (y)
                         not_implemented_error(),
                       
                       tf_forward = function (x)
                         not_implemented_error(),
                       
                       tf_log_jacobian = function (x)
                         not_implemented_error(),
                       
                       str = function (x, ...)
                         not_implemented_error()
                       
                     ))

Identify <- R6Class('Identity',
                    inherit = Transform,
                    public = list(
                      
                      forward = function (x)
                        x,
                      
                      backward = function (y)
                        y,
                      
                      tf_forward = function (x)
                        tf$identity(x),
                      
                      tf_log_jacobian = function (x)
                        tf$zeros(shape(1),tf$float64),
                      
                      str = function (x, ...)
                        'none'
                      
                    ))

Exp <- R6Class('Exp',
               inherit = Transform,
               public = list(
                 
                 .lower = NULL,
                 
                 initialize = function (lower = 1e-6)
                   self$.lower <- lower,
                 
                 forward = function (x)
                   exp(x) + self$.lower,
                 
                 backward = function (y)
                   log(y - self$.lower),
                 
                 tf_forward = function (x)
                   tf$exp(x) + self$.lower,
                 
                 tf_log_jacobian = function (x)
                   tf$reduce_sum(x),
                 
                 str = function (x, ...)
                   '+ve'
                 
               ))

# A transform of the form
# y = \log ( 1 + \exp(x))
# x is a free variable, y is always positive.
Log1pe <- R6Class('Log1pe',
                  inherit = Transform,
                  public = list(
                    
                    .lower = NULL,
                    
                    # lower is a float that defines the minimum value that this
                    # transform can take, default 1e-6. This helps stability during
                    # optimization, because aggressive optimizers can take
                    # overly-long steps which lead to zero in the transformed
                    # variable, causing an error.
                    initialize = function (lower = 1e-6)
                      self$.lower <- lower,
                    
                    forward = function (x)
                      log1p(exp(x)) + self$.lower,
                    
                    backward = function (y)
                      log(exp(y - self$.lower) - 1),
                    
                    tf_forward = function (x) {
                      one <- tf$ones(tf$shape(x), tf$float64)
                      tf$log(one + tf$exp(x)) + self$.lower 
                    },
                    
                    tf_log_jacobian = function (x)
                      -tf$reduce_sum(tf$log(1 + tf$exp(-x))),
                    
                    str = function (x, ...)
                      '+ve'
                    
                  ))

Logistic <- R6Class('Logistic',
                    inherit = Transform,
                    public = list(
                      
                      a = NULL,
                      b = NULL,
                      .a = NULL,
                      .b = NULL,
                      
                      initialize = function (a = 0, b = 1) {
                        super$initialize()
                        stopifnot(b > a)
                        self$a <- a
                        self$b <- b
                        self$.a <- tf$constant(a, tf$float64)
                        self$.b <- tf$constant(b, tf$float64)
                      },
                      
                      forward = function (x) {
                        ex <- exp(-x)
                        self$a + (self$b - self$a) / (1 + ex)
                      },
                      
                      backward = function (y)
                        -log((self$b - self$a) / (y - self$a) - 1),
                      
                      tf_forward = function (x) {
                        ex <- tf$exp(-x)
                        self$.a + (self$.b - self$.a) / (1 + ex)
                      },
                      
                      tf_log_jacobian = function (x) {
                        tf$reduce_sum(x - 2 * tf$log(tf$exp(x) + 1) +
                                        tf$log(self$.b - self$.a)) 
                      },
                      
                      str = function (x, ...) {
                        sprintf('[%s, %s ]',
                                capture.output(str(a)),
                                capture.output(str(b))) 
                      }
                      
                    ))


positive <- Log1pe
