# `simglm`: Tidy simulation and power analyses

[![R build status](https://github.com/lebebr01/simglm/workflows/R-CMD-check/badge.svg)](https://github.com/lebebr01/simglm/actions?workflow=R-CMD-check)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/lebebr01/simglm?branch=main&svg=true)](https://ci.appveyor.com/project/lebebr01/simglm)
[![codecov.io](https://codecov.io/github/lebebr01/simglm/coverage.svg?branch=main)](https://codecov.io/github/lebebr01/simglm?branch=main)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/simglm)](https://cran.r-project.org/package=simglm)


## Package Installation
This package can be directly installed through CRAN:


```r
install.packages("simglm")
```

The development version of the package can be installed by using the devtools package.


```r
library(devtools)
install_github("lebebr01/simglm")
```

## Introduction to the simglm package
The best way to become oriented with the `simglm` package is through the package vignette. There are two ways to get to the vignettes (both will open a browser to view the vignette). Below is an example loading the "Intro" vignette directly:


```r
browseVignettes()
vignette("Intro", package = "simglm")
```

Note: If you install the development version of the package, you may need to tell R to build the vignettes when installing the `simglm` package by doing the following:

```r
install_github("lebebr01/simglm", build_vignettes = TRUE)
```

## Features

A flexible suite of functions to simulate nested data.  
Currently supports the following features:

* Longitudinal data simulation
* Three levels of nesting
* Specification of distribution of random components (random effects and random error)
* Specification of serial correlation
* Specification of the number of variables
    + Ability to add time-varying covariates
    + Specify the mean and variance of fixed covariate variables
    + Specify floor or ceiling aspects of continuous attributes
    + Factor variable simulation 
    + Ordinal variable simulation
* Generation of mixture normal distributions
* Cross sectional data simulation
* Single level simulation
* Power by simulation
    + Vary parameters for a factorial simulation design.
    + Can vary model fitted to the data to misspecify directly.
* Simulation of missing data
* Include other distributions for covariate simulation.
* Continuous, Logistic (dichotomous), Poisson (count), ordinal (rating scale) outcome variables.
* Cross classified simulation and power

## Bugs/Feature Requests

Bugs and feature requests are welcomed. Please track these on GitHub here: <https://github.com/lebebr01/simglm/issues>. I'm also open to pull requests.

Enjoy!
