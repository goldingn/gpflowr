# autoflow trickery

# This function is designed for use on methods of the Parameterized class 
# (below).
# 
# The idea is that methods that compute relevant quantities (such as
# predictions) can define a tf graph which we automatically run when the
# (decorated) function is called. Not only is the syntax cleaner, but multiple
# calls to the method will result in the graph being constructed only once.
# 
# the function `autoflow()` (below) should be used in the `initialize()` method,
# to overwrite a public method by wrapping it with this

# method_name is a string saying which method to overwrite
# tf_method is the function itself
# envir is the environment of the original method
# tf_arg_tuples is a list of tf dtypes

#' @importFrom utils capture.output
AutoFlow <- function (method_name, tf_method, envir, tf_arg_tuples = list()) {
  # need to work out how to get the name of the method!
  
  storage_name <- sprintf('_%s_AF_storage',
                          method_name)
  
  runnable <- function (...) {
    if (has(self[['.tf_mode_storage']], storage_name)) {
      
      storage <- self[['.tf_mode_storage']][[storage_name]]
      
    } else {
      
      storage <- list()
      storage[['free_vars']] <- tf$placeholder(tf$float64)
      self$make_tf_array(storage$free_vars)
      storage[['tf_args']] <- lapply(tf_arg_tuples, tf$placeholder)
      
      # with self temporarily in tf_mode, execute tf_method on self,
      # optionally including placeholders
      with(self$tf_mode %as% instance,
           storage[['tf_result']] <- do.call(tf_method,
                                             c(instance, storage[['tf_args']])))
      
      # prep the session
      storage[['session']] <- tf$Session()
      storage[['session']]$run(tf$initialize_all_variables(),
                               feed_dict = self$get_feed_dict())
      
      # store the storage object
      self[['.tf_mode_storage']][[storage_name]] <- storage
      
    }
    
    # create an appropriate dict
    # align the elements of dots with the tf_arg_tuples to form a feed_dict
    R_args <- list(...)
    names(R_args) <- unlist(tf_arg_tuples)
    feed_dict <- dict(R_args)
    feed_dict[storage[['free_vars']]] <- self$get_free_state()
    feed_dict <- c(feed_dict, self$get_feed_dict())
    
    # exeecute the method, using the newly created dict
    storage[['session']]$run(storage[['tf_result']], feed_dict = feed_dict)
    
  }
  
  environment(runnable) <- envir
  runnable
}


# function to apply AutoFlow to an already defined method in an R6 generator's 
# initialize() method. dots accepts dtype objects to create placeholders for the
# arguments of the method being overwritten
autoflow <- function(name, ...) {
  
  # create the dtype list as a character string
  dtypes <- deparse(substitute(list(...)))
  dtype_string <- ifelse(dtypes == 'list()',
                         '',
                         sprintf(', %s', dtypes))
  
  # in the parent environment, unlock the method, replace it with the autoflowed
  # version (avoiding shallow copy), then relock it
  txt <- sprintf("unlockBinding('%s', self)
                 tf_method <- self$%s
                 envir <- environment(self$%s)
                 self$%s <- AutoFlow('%s', tf_method, envir%s)
                 lockBinding('%s', self)",
                 name,
                 name,
                 name,
                 name, name, dtype_string,
                 name)
  
  eval(parse(text = txt),
       envir = parent.frame())
}
