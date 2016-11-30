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

# name is a string saying which method to overwrite
# dots enables the user to pass a list of placeholders corresponding to the arguments of the mehtod

# function to apply AutoFlow to an already defined method in an R6 generator's 
# initialize() method. dots accepts dtype objects to create placeholders for the
# arguments of the method being overwritten
autoflow <- function(name, ...) {
  
  # list of placeholder tensors
  placeholder_list <- list(...)

  # grab the R6 object and the function we're overwriting  
  self <- parent.frame()$self
  tf_method <- self[[name]]
  
  # create a storage name
  storage_name <- sprintf('_%s_AF_storage', name)

  # define the function  
  runnable <- function (...) {
    
    # if it's already defined, grab the graph and session
    if (has(self[['.tf_mode_storage']], storage_name)) {
      
      storage <- self[['.tf_mode_storage']][[storage_name]]
      
    } else {
      # otherwise, build the graph and session
      
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
    feed_dict <- dictify(placeholder_list, list(...))
    
    # execute the method, using the newly created dict
    storage[['session']]$run(storage[['tf_result']],
                             feed_dict = feed_dict)
    
  }

  # unlock the method, assign the new function, and relock
  unlockBinding(name, self)
  self[[name]] <- runnable
  lockBinding(name, self)
  
}
