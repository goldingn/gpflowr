# define Python-esque decorator syntax via an S3 class. Operator syntax
# influenced by the R module: https://github.com/klmr/decorator/

# register generic
decorate <- function (decorator, fun, ...) {
  UseMethod(decorate)
}

# operator for nicer syntax
# i.e. foo <- dec %=% function() {}
# rather than foo <- decorate(dec, function() {})
# or foo <- dec(function() {})
`%=%` <- decorate

# error on anything other than an object of class decorator
decorate.default <- function (decorator, fun, ...) {
    stop ('not a decorator')
}

# apply the decorator to the function, and return a decorated function
decorate.decorator <- function (decorator, fun, ...) {
  function (...) {
    decorator(fun(...))
  }
}

# coerce a function to a decorator
as.decorator <- function (fun) {
  stopifnot (inherits(cl, 'function'))
  class(fun) <- c('decorate', class(fun))
  fun
}

# minimal print method
print.decorator <- function (x, ...) {
  cat('decorator function\n')
}
