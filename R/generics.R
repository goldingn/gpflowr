# generic methods for GPflow

# generic addition and multiplication for kernels
#' @export
`+.GPflow.kernels.Kern` <- function(a, b)
  gpflow$kernels$Add(list(a, b))

#' @export
`*.GPflow.kernels.Kern` <- function(a, b)
  gpflow$kernels$Prod(list(a, b))





# simple cloning of environments
clone_env = function (env) {
  # make a new environment in the same position
  new_env <- new.env(parent.env(env), hash = FALSE)
  
  # copy over everything in it
  elements <- as.list.environment(env, all.names = TRUE)
  new_env <- list2env(elements, new_env)
  attributes(new_env) <- attributes(env)
  
  new_env
}

# remove all of the GPflow classes from an object
remove_gpflow_classes <- function (x) {
  classes <- class(x)
  class(x) <- classes[-grep('^GPflow', classes)]
  x
}

# remove the boldface characters from python printing
purge_python_boldface <- function (text) {
  text <- gsub('\033', '', text)
  text <- gsub('\\[1m', '', text)
  text <- gsub('\\[0m', '', text)
  text
}

# clean up the str() method for Parentable objects
#' @export
str.GPflow.param.Parentable <- function (x, ...) {
  
  # clone the object, strip gpflow classes from it, & capture output of str()
  # (to stop recursion without calling an unexported function from reticulate)
  x_tmp <- clone_env(x)
  x_tmp <- remove_gpflow_classes(x_tmp)
  out <- capture.output(str(x_tmp, ...))
  
  # remove boldface from python output & dump to the console
  out_clean <- purge_python_boldface(out)  
  cat(paste(out_clean, sep = '\n'), sep = '\n')
  
  invisible(NULL)
  
}
