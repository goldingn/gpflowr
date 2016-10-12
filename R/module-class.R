# a module class, to wrap up R6 constructors of each class

# construct a module from a list of arguments. Uses some magic to avoid having
# to name all the arguments or specifying the $new method

module <- function (...) {
  
  # construct the list, autofill the names
  element_list <- list(...)
  names(element_list) <- get_names(...)
  
  # if an element is an R6 generator, grab the $new method
  element_list <- lapply(element_list, get_new)
  
  # check and coerce
  as.module(element_list)
  
}

# check an object can be coerced to a module
check_module_content <- function (x) {
  
  if (!inherits(x, 'list'))
    stop ('modules can only be created from lists')
  
  if (length(names(x)) < length(x)) 
    stop ('all module elements must be named')

  if (any(duplicated(names(x)))) 
    stop ('all module element names must be unique')
  
}

# if x is an R6 generator replace with its $new method
get_new <- function (x) {
  if (inherits(x, 'R6ClassGenerator'))
    x <- x$new
  x
}

# find the names of the arguments in dots
get_names <- function (...) {
  
  # grab the arguments, and pull out the objects
  args <- as.list(substitute(list(...)))[-1L]
  
  # if one of the arguments was named as something else (e.g. a = x), get the
  # *name* rather than the object
  names <- names(args)
  named <- which(names != '')
  
  for (i in named) {
    args[[i]] <- names[i]
  }
  
  # convert to a vector
  args <- unlist(args)
  names(args) <- NULL
  args
}

# assign module class
as.module <- function (x) {
  check_module_content(x)
  class(x) <- c('module', class(x))
  x
}

# test class membership
is.module <- function (x) {
  inherits(x, 'module')
}

# basic print method
print.module <- function (x, ...) {
  members <- paste(names(x), collapse = ', ')
  msg <- sprintf('module with members:\n  %s\n',
                 members)
  cat(msg)
}