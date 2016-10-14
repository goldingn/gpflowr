# define R6 classes: Parentable, Param, DataHolder, Parameterized and ParamList
# port of GPflow/GPflow/param.py

# ' @title Parentable class
# '
# ' @description A very simple class for objects in a tree, where each node contains a
# ' reference to '_parent'.
# '
# ' @details This class can figure out its own name (by seeing what it's called by the
# ' _parent's __dict__) and also recurse up to the highest_parent.
# '
Parentable <- R6Class('Parentable',
                      public = list(
                        
                        # enclosing env of parent
                        .parent_env = NULL,
                        
                        # own hex in parent's .hex_list 
                        .hex = NULL,
                        
                        # named list of shas of children
                        .hex_list = list(),
                        
                        # set the environment of the parent
                        .set_parent_env = function (parent)
                          self$.parent_env <- parent$.__enclos_env__,
                        
                        # bespoke assignment, to build tree structure                        
                        `$<-` = function (x, i, value) {
                          # when setting values, if the new value is a parentable,
                          # link the child (value) and parent (self)
                          
                          if (inherits(value, 'Parentable')){
                            
                            # generate unique hex to index the child in the parent
                            hex <- get_hex()
                            value[['.hex']] <- hex
                            self[['.hex_list']][[i]] <- hex
                            
                            # and give the child the environment of the parent
                            value[['.set_parent_env']](self)
                            
                          }
                          
                          # either way, assign the value
                          self[[i]] <- value
                          
                          self
                          
                        },
                        
                        # get the index to a child
                        which_child = function (child) {
                          
                          if (!inherits(child, 'Parentable'))
                            stop ('Parentables can only have children that are also Parentables')
                          
                          # return the index, retaining its name in the hex list
                          idx <- match(child$.hex, self$.hex_list)
                          names(idx) <- names(self$.hex_list)[idx]
                          idx
                          
                        },
                        
                        print = function (...) {
                          # find the classes to which this object belongs and print them
                          
                          classes <- class(self$clone())
                          
                          main_class <- classes[1]
                          other_classes <- classes[-1]
                          other_classes <- other_classes[other_classes != 'R6']
                          
                          if (length(other_classes) > 0) {
                            inheritance_msg <- sprintf('(inheriting from %s)\n',
                                                       paste(other_classes,
                                                             collapse = ' < '))
                          } else {
                            inheritance_msg <- ''
                          }
                          
                          msg <- sprintf('%s object\n%s',
                                         main_class,
                                         inheritance_msg)
                          cat (msg)
                          
                        }
                        
                        # used for pickling, so ignore for now
                        # would need to write a dict class
                        # .getstate <- function () {
                        # # get list of elements, remove parent, return list?
                        #   d <- self$.dict
                        #   d$pop('_parent')
                        #   return (d)
                        # },
                        # .setstate = function (d) {
                        # # replace list of elements with new list of elements, then remove parent?
                        #   self$.dict$update(d)
                        #   self$parent <- NULL
                        # }
                        
                      ),
                      
                      active = list(
                        
                        # make name and long_name properties
                        
                        name = function (value) {
                          
                          # let the user know they can't assign names in this way
                          if (!missing(value))
                            warning ('name assignment ignored')
                          
                          # An automatically generated name, given by the
                          # reference of the _parent to this instance
                          if (is.null(self$parent))
                            return ('unnamed')
                          
                          # get the index
                          idx <- self$parent$which_child(self)
                          
                          if (inherits(self$parent, 'ParamList'))
                            return (sprintf('item%i', self$parent$.list$index(self)))
                          
                          if (length(idx) == 0)
                            stop("mis-specified parent. This Param's .parent does not contain a reference to it.")
                          
                          if (length(idx) > 1)
                            stop("This Param appears to be doubly referenced by a parent")
                          
                          names(idx)
                        },
                        
                        long_name = function (value) {
                          # This is a unique identifier for a param object 
                          # within a structure, made by concatenating the names 
                          # through the tree.
                          
                          # let the user know they can't assign names in this way
                          if (!missing(value))
                            warning ('name assignment ignored')
                          
                          if (is.null(self$parent))
                            return (self$name)
                          
                          paste(self$parent$long_name,
                                self$name,
                                sep = '$')
                        },
                        
                        parent = function (value) {
                          # get the parent object from its environment
                          if (missing(value))
                            self$.parent_env$self
                          else
                            self$.parent_env$self <- value
                        },
                        
                        highest_parent = function (value) {
                          # A reference to the top of the tree, usually a Model
                          # instance
                          if (missing(value)) {
                            if (is.null(self$parent))
                              self
                            else
                              self$parent$highest_parent
                          } else {
                            if (is.null(self$parent))
                              self <- value
                            else
                              self$parent$highest_parent <- value
                          }
                        }
                        
                      ))

Param <- R6Class('Param',
                 inherit = Parentable,
                 
                 public = list(
                   
                   .array = NULL,
                   .tf_array = NULL,
                   .log_jacobian = NULL,
                   prior = NULL,
                   transform = NULL,
                   fixed = FALSE,
                   
                   initialize = function (array, transform = transforms$Identity()) {
                     self$value <- as.array(array)
                     self$transform <- transform
                   },
                   
                   # get_parameter_dict = function (d)
                   #   d[[self$long_name]] <- self$value,
                   #
                   # set_parameter_dict = function (d)
                   #   self$value <- d[[self$long_name]],
                   
                   # get_samples_df = function (samples) {
                   #   # Given a numpy array where each row is a valid free-state
                   #   # vector, return a pandas.DataFrame which contains the
                   #   # parameter name and associated samples in the correct form
                   #   # (e.g. with positive constraints applied).
                   #   # if (self$fixed)
                   #     # return (pd.Series([self.value for _ in range(samples.shape[0])], name=self.long_name))
                   #   start <- self$highest_parent()$get_param_index(self)[1]
                   #   end <- start + self$size - 1
                   #   samples <- samples[, start:end]
                   #   # samples <- samples.reshape((samples.shape[0],) + self.shape)
                   #   samples <- self$transform$forward(samples)
                   #   # return (pd.Series([v for v in samples], name=self.long_name))
                   # },
                   
                   make_tf_array = function (free_array) {
                     # free_array is a tensorflow vector which will be the optimisation
                     # target, i.e. it will be free to take any value.
                     # Here we take that array, and transform and reshape it so that it can be
                     # used to represent this parameter
                     # Then we return the number of elements that we've used to construct the
                     # array, so that it can be sliced for the next Param.
                     
                     # fixed parameters are treated by tf.placeholder
                     if (self$fixed)
                       return (0)
                     free_size <- self$size
                     x_free <- free_array[1:free_size]
                     mapped_array <- self$transform$tf_forward(x_free)
                     self$.tf_array <- tf$reshape(mapped_array, self$shape)
                     self$.log_jacobian <- self$transform$tf_log_jacobian(x_free)
                     return (free_size)
                   },
                   
                   get_free_state = function () {
                     # Take the current state of this variable, as stored in
                     # self.value, and transform it to the 'free' state. This is
                     # a numpy method.
                     if (self$fixed)
                       return (0)
                     return (self$transform$backward(self$value))
                   },
                   
                   # get_feed_dict = function() {
                   #    # Return a dictionary matching up any fixed-placeholders to their values
                   #    d <- list()
                   #    if (self$fixed)
                   #      d[[self$.tf_array]] <- self$value
                   #    return (d)
                   # },
                   
                   set_state = function (x) {
                     # Given a vector x representing the 'free' state of this Param, transform
                     # it 'forwards' and store the result in self._array. The values in
                     # self._array can be accessed using self.value
                     # This is a numpy method.
                     if (self$fixed)
                       return (0)

                     new_x <- self$transform$forward(x)
                     new_array <- array(new_x, dim = self$shape)
                     stopifnot(all(dim(new_array) == dim(self$.array)))
                     self$.array <-  new_array
                     return (self$size)
                   },
                   
                   
                   build_prior = function () {
                     # Build a tensorflow representation of the prior density.
                     # The log Jacobian is included.
                     if (is.null(self$prior))
                       return (tf$constant(0.0, float_type))
                     else if (is.null(self$.tf_array))  # pragma: no cover
                       stop ("tensorflow array has not been initialized")
                     else
                       return (self$prior$logp(self$.tf_array) + self$.log_jacobian)
                   }#,
                   # 
                   #                    `$<-` = function (x, i, value) {
                   #                      # When some attributes are set, we need to recompile the tf model before
                   #                      # evaluation.
                   #                      self[[i]] <- value
                   #                      if (i %in% recompile_keys)
                   #                        self$highest_parent$.needs_recompile <- TRUE
                   # 
                   #                      # when setting the fixed attribute, make or remove a placeholder appropraitely
                   #                       if (i == 'fixed') {
                   #                         if (value)
                   #                           self$.tf_array <- tf$placeholder(dtype = float_type,
                   #                                                           shape = self$.array$shape,
                   #                                                           name = self$name)
                   #                         else
                   #                           self$.tf_array = NULL
                   #                       }
                   #                    },
                   
                   # def __str__(self, prepend=''):
                   #   return prepend + \
                   # '\033[1m' + self.name + '\033[0m' + \
                   # ' transform:' + str(self.transform) + \
                   # ' prior:' + str(self.prior) + \
                   # (' [FIXED]' if self.fixed else '') + \
                   # '\n' + str(self.value)
                   
                   # getstate = function (self) {
                   #   d <- super$getstate()
                   #   d$pop('_tf_array')
                   #   d$pop('_log_jacobian')
                   #   return (d)
                   # },
                   # 
                   # setstate = function (self) {
                   #   super$setstate(d)
                   #   self$.log_jacobian <- NULL
                   #   self$fixed <- self$fixed
                   # }
                 ),
                 
                 # point 'value' at the array
                 active = list(
                   value = property('.array'),
                   shape = function (value) dim(self$.array),
                   size = function (value) prod(self$shape)
                 )
)

# DataHolder <- R6Class('DataHolder',
#                          inherit = Parentable,
#                          public = list(
#
#                          ))

Parameterized <- R6Class('Parameterized',
                         inherit = Parentable,
                         public = list(
                           
                           x = NULL,
                           .tf_mode = FALSE,
                           .tf_mode_storage = list(),
                           
                           initialize = function () {
                             self$.tf_mode <- FALSE
                           },
                           
                           get_parameter_dict = function (d = NULL) {
                             
                             if (is.null(d))
                               d  <- list()
                             
                             for (p in self$sorted_params)
                               p$get_parameter_dict(d)
                             
                             d
                           },
                           
                           set_parameter_dict = function (d) {
                             for (p in self$sorted_params)
                               p$set_parameter_dict(d)
                           },

                           `$` = function (x, i) {
                             # return a tensorflow array if `x` is in tf_mode,
                             # and the object containing that array otherwise
                             # equivalent to python __getattribute__ method
                             o <- x[[i]]
                             
                             if (has(x, '.tf_mode') && x[['.tf_mode']] && has(o, '.tf_array'))
                               o <- o[['.tf_array']]
                             
                             o
                           },
                           
                           # `$<-` = function (x, i, value) {
                           #
                           # },
                           
                           .kill_autoflow = function () {
                             # remove all AutoFlow storage dicts recursively
                             self$.tf_mode_storage <- list()
                             
                             for (i in seq_len(self$sorted_params)) {
                               if (inherits(self$sorted_params[[i]]))
                                 self$sorted_params[[i]]$.kill_autoflow()
                             }
                              
                           },
                           
                           make_tf_array = function (X) {
                             # X is a tf placeholder. It gets passed to all the
                             # children of this class (that are Parameterized or
                             # Param objects), which then construct their
                             # tf_array variables from consecutive sections.
                             nrow <- X$get_shape()$as_list()[1]
                             
                             count <- 0
                             for (i in seq_along(self$sorted_params))
                               count  <- count + self$sorted_params[[i]]$make_tf_array(X[count:nrow])
                               
                             count
                           },
                           
                           get_free_state = function () {
                             # recurse get_free_state on all child parameters, and hstack them.
                             free_states <- lapply(self$sorted_params,
                                              function(x) x$get_free_state())
                             array(do.call(c, free_states))
                           },

                           get_feed_dict = function () {
                             # Recursively fetch a dictionary matching up fixed-placeholders to
                             # associated values
                             lapply(c(self$sorted_params, self$data_holders),
                                    function(x) x$get_feed_dict())
                           },

                           set_state = function (x) {
                             # Set the values of all the parameters by recursion
                             nrow <- x$get_shape()$as_list()[1]
                             
                             count <- 0
                             for (name in names(self$sorted_params))
                               count  <- count + self$sorted_params[[name]]$set_state(x[count:nrow])
                             
                             count
                           },
                           
                           tf_mode = function () {
                             on.exit(self$.end_tf_mode())
                             self$.begin_tf_mode()
                             return (self$clone())
                           },
                           
                           .begin_tf_mode = function () {
                             self$.tf_mode <- TRUE
                           },
                           
                           .end_tf_mode = function () {
                             self$.tf_mode <- FALSE
                           },

                           build_prior = function () {
                             # Build a tf expression for the prior by summing all child-node priors.
                             nparam <- length(self$sorted_params)
                             pri <- self$sorted_params[[1]]$build_prior()
                             
                             if (nparam > 0) {
                               for (i in 2:nparam)
                                 pri <- pri + self$sorted_params[[i]]$build_prior()
                             }
                           }#,
                           # 
                           # str = function (object, prepend = '') {
                           #   
                           # },
                           # 
                           # .html_table_rows = function (name_prefix = '') {
                           #   
                           # },
                           # 
                           # .repr_html_ = function () {
                           #   
                           # },
                           # 
                           # .__setstate__ = function (d) {
                           #   
                           # }
                           
                           ),
                         active = list(
                           
                           sorted_params = function (value) {
                             # Return a list of all the child parameters, sorted by id. This makes
                             # sure they're always in the same order.
                             
                             if (!missing(value))
                               warning ('assignment ignored')
                             
                             # find names of elements
                             names <- names(self)
                             names <- names[names != 'parent']
                             
                             # pull out those that are Param-esque
                             params <- list()
                             for (name in names) {
                               if (inherits(self[[name]], c('Param', 'Parameterized')))
                                 params[[name]] <- self[[name]]
                             }
                             
                             # order them
                             params <- params[order(names(params))]
                               
                           },
                           
                           data_holders = function (value) {
                             # Return a list of all the child DataHolders                             
                             if (!missing(value))
                               warning ('assignment ignored')
                             
                             params <- list()
                             for (name in names(self)) {
                               if (inherits(self[[name]], 'DataHolder'))
                                 params[[name]] <- self[[name]]
                             }
                             params
                           },
                           
                           fixed = function (value) {
                             
                             if (!missing(value)) {
                               
                               for (name in names(self))
                                 self[[name]]$fixed <- value
                               
                             } else {
                               
                               ans <- vapply(self$sorted_params,
                                             function(x) x$fixed,
                                             FALSE)
                               
                               return (ans)
                               
                             }
                             
                           }
                           
                         ))


ParamList <- R6Class('ParamList',
                     inherit = Parameterized,
                     public = list(

                       
                     ))

