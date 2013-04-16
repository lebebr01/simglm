\name{fixef.sim.nested}
\alias{fixef.sim.nested}
\title{Simulates design matrix.}
\usage{
  fixef.sim.nested(fixed, fixed.vars, n, p, w.var,
    data.str)
}
\arguments{
  \item{fixed}{One sided formula for fixed effects in the
  simulation, currently assumes intercept.}

  \item{fixed.vars}{Character vector of covariates for
  design matrix.}

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

