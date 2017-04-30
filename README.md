# gpflowr 

[![Build Status](https://travis-ci.org/goldingn/gpflowr.svg?branch=master)](https://travis-ci.org/goldingn/gpflowr)
[![codecov.io](https://codecov.io/github/goldingn/gpflowr/coverage.svg?branch=master)](https://codecov.io/github/goldingn/gpflowr?branch=master)
[![cran version](http://www.r-pkg.org/badges/version/gpflowr)](https://cran.rstudio.com/web/packages/gpflowr)

gpflowr (jee-pee-flower) is a simple R import of the [GPflow python package](github.com/gpflow/gpflow#README.md) for fitting Gaussian process models using Google's [TensorFlow library](https://www.tensorflow.org/).

The package is a very thin wrapper around a [reticulate](https://github.com/rstudio/reticulate) import of GPflow. The package provides a few convenience functions, but still requires some knowledge of python to use.

#### Installation

`gpflowr` isn't yet on CRAN, and also depends on a version of `reticulate` that isn't on CRAN. To install `gpflowr`, you can do:

```r
devtools::install_github('rstudio/reticulate')
devtools::install_github('goldingn/gpflowr')
```

#### Contributing

If you spot something about the port that you think we could improve, please let us know!
