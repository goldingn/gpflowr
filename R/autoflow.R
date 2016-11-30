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
# placeholder_list is a list of placeholders (rank but not shape defined)

#' @importFrom utils capture.output
AutoFlow <- function (method_name, tf_method, placeholder_list) {
  
  storage_name <- sprintf('_%s_AF_storage',
                          method_name)
  
  runnable <- function (...) {
    
    R_args <- list(...)
    
    if (has(self[['.tf_mode_storage']], storage_name)) {
      
      storage <- self[['.tf_mode_storage']][[storage_name]]
      
    } else {
      
      storage <- list()
      storage[['session']] <- tf$Session()
      
      storage[['tf_args']] <- placeholder_list
      storage[['free_vars']] <- tf$placeholder(tf$float64, shape(NULL))
      
      self$make_tf_array(storage[['free_vars']])
      storage[['tf_result']] <- do.call(tf_method, storage[['tf_args']])
             
      # store the storage object for next time
      self[['.tf_mode_storage']][[storage_name]] <- storage
      
    }
    
    # create an appropriate dict
    feed_dict <- dictify(placeholder_list, R_args)
    
    # exeecute the method, using the newly created dict
    storage[['session']]$run(storage[['tf_result']],
                             feed_dict = feed_dict)
    
  }
  
  # for some reason, the lexical scoping is broken, and these objects aren't
  # visible to runnable, so define them explicilty
  envir <- environment(tf_method)
  envir$placeholder_list <- placeholder_list
  envir$tf_method <- tf_method
  envir$storage_name <- storage_name
  environment(runnable) <- envir
  
  runnable
}


# function to apply AutoFlow to an already defined method in an R6 generator's 
# initialize() method. dots accepts dtype objects to create placeholders for the
# arguments of the method being overwritten
autoflow <- function(name, ...) {
  
  # list of placeholder tensors
  placeholder_list <- list(...)

  # grab the R6 object, and the function we're overwriting  
  self <- parent.frame()$self
  tf_method <- self[[name]]
  
  # create a new environment to put the method in
  envir <- new.env()
  environment(tf_method) <- envir
  
  af_method <- AutoFlow(name, tf_method, placeholder_list)
  
  # unlock the method, assign the new function, and reassign
  unlockBinding(name, self)
  self[[name]] <- af_method
  lockBinding(name, self)
  
  self

}
