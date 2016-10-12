# gpflowr 

[![Build Status](https://travis-ci.org/goldingn/gpflowr.svg?branch=master)](https://travis-ci.org/goldingn/gpflowr)
[![codecov.io](https://codecov.io/github/goldingn/gpflowr/coverage.svg?branch=master)](https://codecov.io/github/goldingn/gpflowr?branch=master)
[![cran version](http://www.r-pkg.org/badges/version/gpflowr)](https://cran.rstudio.com/web/packages/gpflowr)

gpflowr (jee-pee-flower) is an R port of the [GPflow python package](github.com/gpflow/gpflow#README.md) for fitting Gaussian process models using Google's [TensorFlow library](https://www.tensorflow.org/).
The aim is to make this a pretty close translation of the python code, so it uses [R6 classes](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html) for object-oriented design, with some tweaks and hacks to mimic python behaviour and functionality.

R doesn't have an equivalent to [GPflow's readthedocs site](http://gpflow.readthedocs.io), so the package documentation is hosted [here](https://goldingn.github.io/gpflowr) (with a little help from [pkgdown](https://github.com/hadley/pkgdown))

The package is currently only in the very early stages of development, so watch this space!
