# a module class, to wrap up R6 constructors of each class

# construct a module from a list of arguments
module <- function (...) {
  as.module(list(...))
}

# check an objectcan be coerced to a module
check_module_content <- function (x) {
  
  if (!inherits(x, 'list'))
    stop ('modules can only be created from lists')
  
  if (length(names(x)) < length(x)) 
    stop ('all module elements must be named')

  if (any(duplicated(names(x)))) 
    stop ('all module element names must be unique')
  
}


# assign class module
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