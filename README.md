# gpflowr 

[![Build Status](https://travis-ci.org/goldingn/gpflowr.svg?branch=master)](https://travis-ci.org/goldingn/gpflowr)
[![codecov.io](https://codecov.io/github/goldingn/gpflowr/coverage.svg?branch=master)](https://codecov.io/github/goldingn/gpflowr?branch=master)
[![cran version](http://www.r-pkg.org/badges/version/gpflowr)](https://cran.rstudio.com/web/packages/gpflowr)

gpflowr (jee-pee-flower) will be an R import of the [GPflow python package](github.com/gpflow/gpflow#README.md) for fitting Gaussian process models using Google's [TensorFlow library](https://www.tensorflow.org/).

The package is about to be completely overhauled, deleting almost all of the code to make this a very thin wrapper around importing GPflow using Rstudio's [reticulate](https://github.com/rstudio/reticulate) package.
So if you're interested in using GPflow in R, you should instead install GPflow for python, and import it using reticulate.

