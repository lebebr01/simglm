# Regression simulation function

[![Build Status](https://travis-ci.org/lebebr01/simglm.svg?branch=master)](https://travis-ci.org/lebebr01/simglm)
[![codecov.io](https://codecov.io/github/lebebr01/simglm/coverage.svg?branch=master)](https://codecov.io/github/lebebr01/simglm?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/simglm)](http://cran.r-project.org/package=simglm)

A flexible suite of functions to simulate nested data.  
Currently supports the following features:
* Longitudinal data simulation
* Three levels of nesting
* Specification of distribution of random components (random effects and random error)
* Specification of serial correlation
* Specification of the number of variables
    + Ability to add time-varying covariates
    + Specify the mean and variance of fixed covariate variables
    + Factor variable simulation 
    + Ordinal variable simulation
* Generation of mixture normal distributions
* Cross sectional data simulation
* Single level simulation
* Power by simulation
    + Vary parameters for a factorial simulation design.
* Simulation of missing data


Features coming soon:
* Include missing data in power simulation designs.
* More options for simulating random components
 * Ability to simulate different distributions for different random effects
* Ability to specify correlation amount random effects individually.
* Expand variance of mixture distribution function to include unequal weighting.

## Package Installation
This package can be installed by using the devtools package.


```r
library(devtools)
install_github("lebebr01/simglm")
library(simglm)
```

## Introduction to the simglm package
The best way to become oriented with the `simglm` package is through the package vignette. There are two ways to get to the vignette (both will open a browser to view the vignette):


```r
browseVignettes()
vignette("Intro", package = "simglm")
```

Note: You may need to tell R to build the vignettes when installing the `simglm` package by doing the following:

```r
install_github("lebebr01/simglm", build_vignettes = TRUE)
```

Enjoy!
