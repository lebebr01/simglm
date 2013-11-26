\name{sim.fixef.nested}
\alias{sim.fixef.nested}
\title{Simulates design matrix.}
\usage{
  sim.fixef.nested(fixed, fixed.vars, cov.param, n, p,
    w.var, data.str)
}
\arguments{
  \item{fixed}{One sided formula for fixed effects in the
  simulation, currently assumes intercept.}

  \item{fixed.vars}{Character vector of covariates for
  design matrix.}

  \item{cov.param}{List of mean and variance for fixed
  effects. Does not include intercept, time, or
  interactions. Must be same order as fixed formula above.}

  \item{n}{Number of clusters.}

  \item{p}{Number of within cluster units.}

  \item{w.var}{Number of time varying covariates or level
  one covariates for cross-sectional clustering. This
  number includes the intercept and time variable for
  longitudinal data.}

  \item{data.str}{Type of data. Must be "cross", or
  "long".}
}
\description{
  Input fixed variables, sample size, and number of within
  variables, returns design matrix.
}
\details{
  Simulates the fixed effects for the \code{\link{sim.reg}}
  function when a linear mixed model is specified.  This
  function assumes a time variable when longitudinal data
  is specified and does include any interactions that are
  specified.
}

